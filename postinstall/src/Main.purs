module Main where

import Prelude

import Effect (Effect)

foreign import copyFolderSync :: String -> String -> Effect Unit
foreign import rmRecursive :: String -> Effect Unit

-- runBuild :: String -> Effect Unit
-- runBuild _ = pure unit

main :: Effect Unit
main = do
  pure unit
  -- initCwd <- Object.lookup "INIT_CWD" <$> Process.getEnv
  -- for_ initCwd \cwd -> do
  --   let filePath = Path.concat [ cwd, "purs-backend-es-processors" ]
  --   backendProcessorsExists <- FS.exists filePath
  --   when backendProcessorsExists do
  --     let externalPath = Path.concat [ cwd, "node_modules", "purs-backend-es", "external" ]
  --     rmRecursive externalPath
  --     copyFolderSync filePath externalPath
  --     runBuild cwd
