-- | The logger interface module. It should not define a specific
-- implementation.
module Logger
  ( Handle (..),
    Level (..),
    logDebug,
    logInfo,
    logWarning,
    logError,
    (.<),
  )
where

import qualified Data.Text as T

-- | The logger handle. This is a public logger interface that can
-- have different implementations. You can use it everywhere.
newtype Handle m = Handle
  { hLowLevelLog :: Level -> T.Text -> m ()
  }

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

logDebug, logInfo, logWarning, logError :: Handle m -> T.Text -> m ()
logDebug h = hLowLevelLog h Debug
logInfo h = hLowLevelLog h Info
logWarning h = hLowLevelLog h Warning
logError h = hLowLevelLog h Error

-- | Concatenates a text and an instance of 'Show'. This is a
-- convenience function to make logger function applications more
-- concise:
--
-- > Log.logError (hLogger h) "The error code is " .< e
(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> T.pack (show a)

infixr 7 .<
