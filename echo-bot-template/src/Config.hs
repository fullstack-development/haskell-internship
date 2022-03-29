-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = error "Not implemented"

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = error "Not implemented"

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = error "Not implemented"
