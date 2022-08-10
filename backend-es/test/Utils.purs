module Test.Utils where

import Prelude

import Data.Either (Either(..))
import Data.Posix.Signal (Signal(..))
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer, freeze)
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (ExecResult, Exit(..), defaultExecOptions, defaultSpawnOptions, inherit)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Node.Stream as Stream

spawnFromParent :: String -> Array String -> Aff Unit
spawnFromParent command args = makeAff \k -> do
  childProc <- ChildProcess.spawn command args defaultSpawnOptions { stdio = inherit }
  ChildProcess.onExit childProc case _ of
    Normally code
      | code > 0 -> Process.exit code
      | otherwise -> k (Right unit)
    BySignal _ ->
      Process.exit 1
  pure $ effectCanceler do
    ChildProcess.kill SIGABRT childProc

execWithStdin :: String -> String -> Aff ExecResult
execWithStdin command input = makeAff \k -> do
  childProc <- ChildProcess.exec command defaultExecOptions (k <<< pure)
  _ <- Stream.writeString (ChildProcess.stdin childProc) UTF8 input mempty
  Stream.end (ChildProcess.stdin childProc) mempty
  pure $ effectCanceler $ ChildProcess.kill SIGABRT childProc

bufferToUTF8 :: Buffer -> Aff String
bufferToUTF8 = liftEffect <<< map (ImmutableBuffer.toString UTF8) <<< freeze
