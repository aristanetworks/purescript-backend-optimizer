module Main where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Either (Either(..), isRight)
import Data.Foldable (oneOf)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, error, makeAff, throwError)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Stream as Stream
import PureScript.Backend.Builder.Cli (basicCliMain)
import PureScript.Backend.Codegen.EcmaScript (esCodegenModule, esForeignModulePath, esModulePath)
import PureScript.CoreFn (Module(..))

main :: Effect Unit
main = basicCliMain
  { name: "purs-backend-es"
  , description: "A PureScript backend for modern ECMAScript."
  , defaultOutputDir: Path.concat [ ".", "output-es" ]
  , onCodegenBefore: mempty
  , onCodegenAfter: mempty
  , onCodegenModule: \args build (Module coreFnMod) backendMod -> do
      let total = show build.moduleCount
      let index = show (build.moduleIndex + 1)
      let padding = power " " (SCU.length total - SCU.length index)
      Console.log $ "[" <> padding <> index <>  " of " <> total <> "] Building " <> unwrap backendMod.name
      let formatted = Dodo.print Dodo.plainText (Dodo.twoSpaces { pageWidth = 180, ribbonRatio = 1.0 }) $ esCodegenModule backendMod
      let modPath = Path.concat [ args.outputDir, esModulePath backendMod.name ]
      writeTextFile UTF8 modPath formatted
      unless (Array.null coreFnMod.foreign) do
        let foreignFileName = esForeignModulePath backendMod.name
        let foreignOutputPath = Path.concat [ args.outputDir, foreignFileName ]
        let origPath = Path.concat [ args.outputDir, "..", coreFnMod.path ]
        let foreignSiblingPath = fromMaybe origPath (String.stripSuffix (Pattern (Path.extname origPath)) origPath) <> ".js"
        res <- attempt $ oneOf
          [ copyFile foreignSiblingPath foreignOutputPath
          , maybe empty (\dir -> copyFile (Path.concat [ dir, esModulePath backendMod.name ]) foreignOutputPath) args.foreignDir
          ]
        unless (isRight res) do
          Console.log $ "  Foreign implementation missing."
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
