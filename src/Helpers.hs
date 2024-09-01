{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class
import Text.Read (readMaybe)

import Discord.Types

echo :: MonadIO m => T.Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> T.Text
showT = T.pack . show

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

getToken :: IO T.Text
getToken = TIO.readFile "./auth-token.secret"

getGuildId :: IO GuildId
getGuildId = do
    gids <- readFile "./guildid.secret"
    case readMaybe gids of
        Just g  -> pure g
        Nothing -> error "could not read guild id from `guildid.secret`"
