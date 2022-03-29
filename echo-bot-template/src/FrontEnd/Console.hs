{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

run :: Handle -> IO ()
run _ = do
  TIO.putStrLn "Welcome to the echo-bot!"
  -- 1. Read a line from the console.
  -- 2. Send it to the bot, get its response and output it.
  -- 3. Go to 1.
  error "Not implemented"
