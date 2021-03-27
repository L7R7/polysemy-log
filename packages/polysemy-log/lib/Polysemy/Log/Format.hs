module Polysemy.Log.Format where

import qualified Data.Text as Text
import GHC.Exception (SrcLoc(..))
import System.Console.ANSI (Color(..), ColorIntensity(Dull), ConsoleLayer(Foreground), SGR (..), setSGRCode)

import Polysemy.Log.Data.LogEntry (LogEntry(LogEntry))
import qualified Polysemy.Log.Data.LogMessage as LogMessage
import Polysemy.Log.Data.LogMessage (LogMessage(LogMessage))
import Polysemy.Log.Data.Severity (Severity(..))

formatSeverity :: Severity -> Text
formatSeverity = \case
  Trace -> "[trace]"
  Debug -> color Green "[debug]"
  Info -> color Blue "[info] "
  Warn -> color Yellow "[warn] "
  Error -> color Red "[error]"
  Crit -> color Magenta "[crit] "
 where
   color c txt =
     toText (setSGRCode [SetColor Foreground Dull c]) <>
     txt <>
     toText (setSGRCode [Reset])

shortModule :: Text -> Text
shortModule =
  spin . Text.splitOn "."
  where
    spin = \case
      [] -> ""
      [m] -> m
      h : t -> Text.take 1 h <> "." <> spin t

formatCaller :: CallStack -> Text
formatCaller =
  maybe "<unknown loc>" format . listToMaybe . getCallStack
  where
    format (_, SrcLoc {..}) =
      [qt|#{shortModule (toText srcLocModule)}##{srcLocStartLine}|]

formatLogEntry :: LogEntry LogMessage -> Text
formatLogEntry (LogEntry LogMessage {..} _ source) =
  [qt|#{formatSeverity severity} [#{formatCaller source}] #{message}|]