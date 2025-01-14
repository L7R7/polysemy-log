{-# language NoImplicitPrelude #-}
{-# options_haddock prune #-}
-- |Description: Polysemy Effects for Logging

module Polysemy.Log (
  -- * Introduction
  -- $intro

  -- * Arbitrary Data Messages
  -- $datalog
  DataLog(DataLog),
  dataLog,

  -- ** Interpreters
  interpretDataLogAtomic',
  interpretDataLogAtomic,

  -- * Text Messages with Severity and Metadata
  -- $messages
  LogMessage(LogMessage),
  LogEntry(LogEntry),
  Log(Log),
  log,
  trace,
  debug,
  info,
  warn,
  error,
  crit,
  formatLogEntry,
  Severity(..),

  -- ** Interpreters
  interpretLogDataLog,
  interpretLogDataLog',
  interpretLogOutput,
  interpretLogNull,
  interpretLogAtomic,
  interpretLogAtomic',

  -- * Concurrent Logging
  interceptDataLogConc,
  interpretLogDataLogConc,
) where

import Polysemy.Log.Atomic (interpretDataLogAtomic, interpretDataLogAtomic', interpretLogAtomic, interpretLogAtomic')
import Polysemy.Log.Conc (interceptDataLogConc)
import Polysemy.Log.Data.DataLog (DataLog(DataLog), dataLog)
import Polysemy.Log.Data.Log (Log(Log), crit, debug, error, info, log, trace, warn)
import Polysemy.Log.Data.LogEntry (LogEntry(LogEntry))
import Polysemy.Log.Data.LogMessage (LogMessage(LogMessage))
import Polysemy.Log.Data.Severity (Severity(..))
import Polysemy.Log.Format (formatLogEntry)
import Polysemy.Log.Log (interpretLogDataLog, interpretLogDataLog', interpretLogDataLogConc)
import Polysemy.Log.Pure (interpretLogNull, interpretLogOutput)

-- $intro
-- There are at least two libraries that wrap a logging backend with polysemy interpreters.
-- An author of a library who wants to provide log messages faces the problem that committing to a backend requires the
-- user to translate those messages if their chosen backend differs.
--
-- /polysemy-log/ provides an abstraction for this task with interpreter adapters for
-- [co-log](https://hackage.haskell.org/package/co-log-polysemy) and
-- [di](https://hackage.haskell.org/package/di-polysemy).
--
-- If you're looking for instructions on how to use /polysemy-log/ with a backend, please visit the haddocks of the
-- adapter libraries:
--
--  - [polysemy-log-co for co-log](https://hackage.haskell.org/package/polysemy-log-co)
--  - [polysemy-log-di for di](https://hackage.haskell.org/package/polysemy-log-di)
--
-- A program using this library might look like this:
--
-- @
-- prog :: Member Log r => Sem r ()
-- prog = do
--   Log.debug "starting"
--   Log.error "nothing happened"
-- @

-- $datalog
-- Logging backends usually don't put any restrictions on the data type that represents a log message, so the adapter
-- effect that faces towards the backend is simply polymorphic in that type.
--
-- For complex logging purposes, it would be perfectly valid to use 'DataLog' directly, even though this library focuses
-- on simpler messages:
--
-- @
-- data ComplexMessage = ComplexMessage { points :: Int, user :: Text }
--
-- prog :: Member (DataLog ComplexMessage) r => Sem r ()
-- prog = do
--   dataLog (ComplexMessage 500 "googleson78")
-- @

-- $messages
-- While it would be quite reasonable to handle any kind of complexly structured logging data ergonomically with
-- /polysemy/, most authors probably prefer not to burden their users with this task while still appreciating the
-- possibility to easily relay debug information in a standardized way.
--
-- The default logging effect uses a simple data structure that annotates the given severity and text message with the
-- source location and timestamp:
