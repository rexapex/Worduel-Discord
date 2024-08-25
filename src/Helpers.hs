{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class
import Text.Read (readMaybe)

import Discord
import Discord.Types
import qualified Discord.Requests as R

echo :: MonadIO m => T.Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> T.Text
showT = T.pack . show

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent

getToken :: IO T.Text
getToken = TIO.readFile "./auth-token.secret"

getGuildId :: IO GuildId
getGuildId = do
    gids <- readFile "./guildid.secret"
    case readMaybe gids of
        Just g  -> pure g
        Nothing -> error "could not read guild id from `guildid.secret`"

-- | Given the test server and an action operating on a channel id, get the
-- first text channel of that server and use the action on that channel.
actionWithChannelId :: GuildId -> (ChannelId -> DiscordHandler a) -> DiscordHandler a
actionWithChannelId testserverid f = do
    Right chans <- restCall $ R.GetGuildChannels testserverid
    (f . channelId) (head (filter isTextChannel chans))
    where
        isTextChannel :: Channel -> Bool
        isTextChannel ChannelText {} = True
        isTextChannel _ = False
