module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Plus (empty)
import Data.Either (Either(..), isRight)
import Data.Foldable (oneOf)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, error, makeAff, throwError)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Stream as Stream
import PureScript.Backend.Optimizer.Builder.Cli (basicCliMain, externalDirectivesFromFile)
import PureScript.Backend.Optimizer.Codegen.EcmaScript (esCodegenModule, esModulePath)
import PureScript.Backend.Optimizer.CoreFn (Module(..), ModuleName(..))

type Args =
  { coreFnDir :: FilePath
  , outputDir :: FilePath
  , foreignDir :: Maybe FilePath
  , directivesFile :: Maybe FilePath
  , intTags :: Boolean
  }

esArgParser :: ArgParser Args
esArgParser =
  ArgParser.fromRecord
    { coreFnDir:
        ArgParser.anyNotFlag "COREFN_DIR"
          "Directory for corefn.json files."
          # ArgParser.default "output"
    , outputDir:
        ArgParser.argument [ "--output-dir" ]
          "Output directory for backend files"
          # ArgParser.default (Path.concat [ ".", "output-es" ])
    , foreignDir:
        ArgParser.argument [ "--foreign-dir" ]
          "Directory for foreign module implementations"
          # ArgParser.optional
    , directivesFile:
        ArgParser.argument [ "--directives" ]
          "Path to file that defines external inline directives"
          # ArgParser.optional
    , intTags:
        ArgParser.flag [ "--int-tags" ]
          "Use integers for tags in codegen instead of strings"
          # ArgParser.boolean
          # ArgParser.default false
    }
    <* ArgParser.flagHelp

main :: FilePath -> Effect Unit
main cliRoot = basicCliMain
  { name: "purs-backend-es"
  , description: "A PureScript backend for modern ECMAScript."
  , argParser: esArgParser
  , resolveCoreFnDirectory: pure <<< _.coreFnDir
  , resolveExternalDirectives: map (fromMaybe Map.empty) <<< traverse externalDirectivesFromFile <<< _.directivesFile
  , onCodegenBefore: \args -> do
      mkdirp args.outputDir
      writeTextFile UTF8 (Path.concat [ args.outputDir, "package.json" ]) esModulePackageJson
      copyFile (Path.concat [ cliRoot, "runtime.js" ]) (Path.concat [ args.outputDir, "runtime.js" ])
  , onCodegenAfter: mempty
  , onCodegenModule: \args build (Module coreFnMod) backendMod@{ name: ModuleName name } -> do
      let formatted = Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 180, ribbonRatio = 1.0 }) $ esCodegenModule { intTags: args.intTags } build.implementations backendMod
      let modPath = Path.concat [ args.outputDir, name ]
      mkdirp modPath
      writeTextFile UTF8 (Path.concat [ modPath, "index.js" ]) formatted
      unless (Set.isEmpty backendMod.foreign) do
        let foreignOutputPath = Path.concat [ modPath, "foreign.js" ]
        let origPath = Path.concat [ args.outputDir, "..", coreFnMod.path ]
        let foreignSiblingPath = fromMaybe origPath (String.stripSuffix (Pattern (Path.extname origPath)) origPath) <> ".js"
        res <- attempt $ oneOf
          [ copyFile foreignSiblingPath foreignOutputPath
          , maybe empty (\dir -> copyFile (Path.concat [ dir, esModulePath backendMod.name ]) foreignOutputPath) args.foreignDir
          ]
        unless (isRight res) do
          Console.log $ "  Foreign implementation missing."
  , onPrepareModule: \_ build coreFnMod@(Module { name }) -> do
      let total = show build.moduleCount
      let index = show (build.moduleIndex + 1)
      let padding = power " " (SCU.length total - SCU.length index)
      Console.log $ "[" <> padding <> index <> " of " <> total <> "] Building " <> unwrap name
      pure coreFnMod
  }

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    res <- Stream.pipe src dst
    Stream.onError src (k <<< Left)
    Stream.onError dst (k <<< Left)
    Stream.onError res (k <<< Left)
    Stream.onFinish res (k (Right unit))
    pure $ effectCanceler do
      Stream.destroy res
      Stream.destroy dst
      Stream.destroy src

mkdirp :: FilePath -> Aff Unit
mkdirp = flip FS.mkdir' { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }

esModulePackageJson :: String
esModulePackageJson = """{"type": "module"}"""
