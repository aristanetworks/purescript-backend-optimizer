module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object as Object
import Node.ChildProcess as CP
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import Sunde as S


foreign import copyFolderSync :: String -> String -> Effect Unit
foreign import rmRecursive :: String -> Effect Unit

main :: Effect Unit
main = do
  initCwd <- Object.lookup "INIT_CWD" <$> Process.getEnv
  for_ initCwd \cwd -> do
    let filePath = Path.concat [ cwd, "purs-backend-es-processors" ]
    backendProcessorsExists <- FS.exists filePath
    if backendProcessorsExists then do
      log "Processors found for purs-backend-es. Recompiling."
      let externalPath = Path.concat [ cwd, "node_modules", "purs-backend-es", "processors", "src" ]
      rmRecursive externalPath
      copyFolderSync filePath externalPath
      liftEffect $ Process.chdir $ Path.concat [ "node_modules", "purs-backend-es" ]
      launchAff_ do
        void $ S.spawn { cmd: "npm", args: ["run", "build"], stdin: Nothing } CP.defaultSpawnOptions
    else
      log "No processors found for purs-backend-es."
