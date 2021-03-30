module Polysemy.Log.Test.LocalTest where

import Polysemy.Test (UnitTest, assertEq, runTestAuto)

import Polysemy.Log.Atomic (interpretDataLogAtomic)
import qualified Polysemy.Log.Data.DataLog as DataLog
import Polysemy.Log.Data.DataLog (DataLog)
import Polysemy.Log.Data.LogMessage (LogMessage(LogMessage))
import Polysemy.Log.Data.Severity (Severity(Debug))
import qualified Data.Map as Map

data Context =
  Context {
    context :: Map.Map Text Text,
    message :: LogMessage
  }
  deriving (Eq, Show)

log ::
  Member (DataLog Context) r =>
  Severity ->
  Text ->
  Sem r ()
log severity msg =
  DataLog.dataLog (Context Map.empty (LogMessage severity msg))

pushContext ::
  Member (DataLog Context) r =>
  (Text, Text) ->
  Sem r a ->
  Sem r a
pushContext (k,v) =
  DataLog.local push
  where
    push (Context c m) =
      Context ((Map.singleton k v) <> c) m

prog ::
  Members [DataLog Context, AtomicState [Context]] r =>
  Sem r [Context]
prog = do
  log Debug "0"
  pushContext ("key1", "foo") do
    log Debug "2"
    pushContext ("key2", "bar") do
      log Debug "3"
  atomicGet

target :: [Context]
target =
  [
    Context (Map.fromList [("key1", "foo"), ("key2", "bar")]) (LogMessage Debug "3"),
    Context (Map.singleton "key1" "foo") (LogMessage Debug "2"),
    Context Map.empty (LogMessage Debug "0")
  ]

test_local :: UnitTest
test_local =
  runTestAuto do
    assertEq @_ @IO target =<< interpretDataLogAtomic @Context prog
