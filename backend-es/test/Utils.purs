module Test.Utils where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, effectCanceler, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, freeze)
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ChildProcess, ExecResult, exitH)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess.Types (Exit(..), stringSignal)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.FS.Aff as FS
import Node.FS.Perms (mkPerms)
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Path (FilePath)
import Node.Process as Process
import Node.Stream (errorH, finishH)
import Node.Stream as Stream

spawnFromParent :: String -> Array String -> Aff Unit
spawnFromParent command args = makeAff \k -> do
  childProc <- spawnImpl command args
  childProc # on_ exitH case _ of
    Normally code
      | code > 0 -> Process.exit' code
      | otherwise -> k (Right unit)
    BySignal _ ->
      Process.exit' 1
  pure $ effectCanceler do
    void $ ChildProcess.kill' (stringSignal "SIGABRT") childProc

execWithStdin :: String -> String -> Aff ExecResult
execWithStdin command input = makeAff \k -> do
  childProc <- ChildProcess.exec' command identity (k <<< pure)
  _ <- Stream.writeString' (ChildProcess.stdin childProc) UTF8 input mempty
  --Stream.end (ChildProcess.stdin childProc) mempty
  (ChildProcess.stdin childProc) # on_ finishH mempty
  pure $ effectCanceler $ void $ ChildProcess.kill' (stringSignal "SIGABRT") childProc

bufferToUTF8 :: Buffer -> Aff String
bufferToUTF8 = liftEffect <<< map (ImmutableBuffer.toString UTF8) <<< freeze

mkdirp :: FilePath -> Aff Unit
mkdirp path = FS.mkdir' path { recursive: true, mode: mkPerms Perms.all Perms.all Perms.all }

-- This is needed because node-child-process is missing the "shell" option.
foreign import spawnImpl :: String -> Array String -> Effect ChildProcess

foreign import loadModuleMainImpl :: (Error -> Effect Unit) -> (Effect Unit -> Effect Unit) -> Effect Unit -> FilePath -> Effect Unit

loadModuleMain :: FilePath -> Aff (Maybe (Effect Unit))
loadModuleMain path = makeAff \k -> do
  loadModuleMainImpl (k <<< Left) (k <<< Right <<< Just) (k (Right Nothing)) path
  mempty

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile from to = do
  stats <- FS.stat from
  unless (Stats.isFile stats) do
    throwError $ error $ "Not a file: " <> from
  --FS.copyFile from to
  makeAff \k -> do
    src <- createReadStream from
    dst <- createWriteStream to
    Stream.pipe src dst
    src # on_ errorH (k <<< Left)
    dst # on_ errorH (k <<< Left)
    dst # on_ finishH (k (Right unit))
    pure $ effectCanceler do
      Stream.destroy dst
      Stream.destroy src
