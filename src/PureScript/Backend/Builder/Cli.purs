module PureScript.Backend.Builder.Cli where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Process as Process
import PureScript.Backend.Builder (BuildEnv, buildModules, coreFnModulesFromOutput)
import PureScript.Backend.Convert (BackendModule)
import PureScript.Backend.Directives (parseDirectiveFile)
import PureScript.Backend.Directives.Defaults as Defaults
import PureScript.CST.Errors (printParseError)
import PureScript.CoreFn (Ann, Module)

type BasicCliArgs =
  { coreFnDir :: FilePath
  , outputDir :: FilePath
  , foreignDir :: Maybe FilePath
  , directivesFile :: Maybe FilePath
  }

basicArgParser :: FilePath -> ArgParser BasicCliArgs
basicArgParser defaultOutputDir =
  ArgParser.fromRecord
    { coreFnDir:
        ArgParser.anyNotFlag "COREFN_DIR"
          "Directory for corefn.json files."
    , outputDir:
        ArgParser.argument [ "--output-dir" ]
          "Output directory for backend files"
          # ArgParser.default defaultOutputDir
    , foreignDir:
        ArgParser.argument [ "--foreign-dir" ]
          "Directory for foreign module implementations"
          # ArgParser.optional
    , directivesFile:
        ArgParser.argument [ "--directives" ]
          "Path to file that defines external inline directives"
          # ArgParser.optional
    }
    <* ArgParser.flagHelp

basicCliMain
  :: { name :: String
     , description :: String
     , defaultOutputDir :: FilePath
     , onCodegenBefore :: BasicCliArgs -> Aff Unit
     , onCodegenAfter :: BasicCliArgs -> Aff Unit
     , onCodegenModule :: BasicCliArgs -> BuildEnv -> Module Ann -> BackendModule -> Aff Unit
     , onPrepareModule :: BasicCliArgs -> BuildEnv -> Module Ann -> Aff (Module Ann)
     }
  -> Effect Unit
basicCliMain options = do
  cliArgs <- Array.drop 2 <$> Process.argv
  let
    parsedArgs =
      ArgParser.parseArgs options.name options.description
        (basicArgParser options.defaultOutputDir)
        cliArgs
  case parsedArgs of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right args@{ coreFnDir, directivesFile } -> launchAff_ do
      externalDirectives <- fromMaybe Map.empty <$> for directivesFile \filePath -> do
        fileContent <- FS.readTextFile UTF8 filePath
        let { errors, directives } = parseDirectiveFile fileContent
        for_ errors \(Tuple directive { position, error }) -> do
          Console.warn $ "Invalid directive [" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "]"
          Console.warn $ "  " <> directive
          Console.warn $ "  " <> printParseError error
        pure directives
      let defaultDirectives = (parseDirectiveFile Defaults.defaultDirectives).directives
      let allDirectives = Map.union externalDirectives defaultDirectives
      coreFnModulesFromOutput coreFnDir >>= case _ of
        Left errors -> do
          for_ errors \(Tuple filePath err) -> do
            Console.error $ filePath <> " " <> err
          liftEffect $ Process.exit 1
        Right coreFnModules -> do
          options.onCodegenBefore args
          coreFnModules # buildModules
            { directives: allDirectives
            , onCodegenModule: options.onCodegenModule args
            , onPrepareModule: options.onPrepareModule args
            }
          options.onCodegenAfter args
