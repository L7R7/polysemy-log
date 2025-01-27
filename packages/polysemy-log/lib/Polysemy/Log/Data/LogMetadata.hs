-- |Description: Internal
{-# options_haddock prune #-}
module Polysemy.Log.Data.LogMetadata where

-- |Internal effect used as an intermediate stage between 'Polysemy.Log.Log' and 'Polysemy.Log.DataLog', for the purpose
-- of isolating the metadata annotation task.
--
-- The type of metadata is arbitrary and chosen in interpreters, but this exposes a 'HasCallStack' dependency since it's
-- the primary purpose.
data LogMetadata a :: Effect where
  -- |Schedule a message to be annotated and logged.
  Annotated :: HasCallStack => a -> LogMetadata a m ()

makeSem ''LogMetadata
