{-# LANGUAGE OverloadedStrings #-}

module Worduel (worduel) where

import Helpers
import Game

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, void)
import Control.Monad.IO.Class
import Data.List (find)
import Data.IORef

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R

data GlobalState = GlobalState  [Game] [UserGame]

worduel :: IO ()
worduel = do
    echo "Worduel!"

    tok <- getToken
    guildId <- getGuildId

    globalState <- newIORef $ GlobalState [] []

    -- Open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
    err <- runDiscord $ def { discordToken = tok
                            , discordOnEvent = onDiscordEvent globalState guildId
                            }
    TIO.putStrLn err

    return ()

data SlashCommand = SlashCommand
    { name :: Text
    , registration :: Maybe CreateApplicationCommand
    , handler :: Interaction -> IORef GlobalState -> Maybe OptionsData -> DiscordHandler ()
    }

mySlashCommands :: [SlashCommand]
mySlashCommands = [ping, challenge]

ping :: SlashCommand
ping = SlashCommand
    { name = "ping"
    , registration = createChatInput "ping" "responds pong"
    , handler = \intr globalState _options ->
        void . restCall $
            R.CreateInteractionResponse (interactionId intr) (interactionToken intr) (interactionResponseBasic "pong") }

challenge :: SlashCommand
challenge = SlashCommand
    { name = "challenge"
    , registration =  createChatInput "challenge" "challenge player to a game of worduel"
    , handler = \intr globalState _options -> do
        GlobalState games users <- liftIO $ readIORef globalState
        let MemberOrUser memberOrUser = interactionUser intr
        let senderId = case memberOrUser  of
                           Left m  -> userId <$> memberUser m
                           Right u -> Just $ userId u
        let guildId = interactionGuildId intr

        case senderId of
            Just sid -> do
                let user = find (\(UserGame uid _) -> uid == sid) users
                case user of
                    Just (UserGame uid gid) -> do
                        let p = Just (Player uid)
                        let updatedGame :: Maybe Game
                            updatedGame = processPlayerAction <$> find (\Game { gameId = gameId } -> gameId == gid) games <*> p
                        liftIO $ writeIORef globalState (GlobalState games users)
                        echo "Failed to create game, user is already in a game!"
                    Nothing -> do
                        -- Create a game, create a player, link the player to the game
                        case guildId of
                            Just gid -> do
                                let game = newGame gid sid sid -- TODO - Currently using requester ID for both players
                                let userGame = UserGame sid (gameId game)

                                void . restCall $
                                    R.CreateInteractionResponse
                                        (interactionId intr)
                                        (interactionToken intr)
                                        (interactionResponseBasic "Starting game of Worduel!")

                                let channelId = interactionChannelId intr
                                case channelId of
                                    Just cid -> do
                                        -- Create the thread where players will send guesses
                                        msgResult <- restCall $ R.CreateMessage cid "Check DMs for your hidden words & enter guesses in the thread"
                                        case msgResult of
                                            Right msg -> do
                                                void . restCall $
                                                    R.StartThreadFromMessage cid (messageId msg) R.StartThreadOpts
                                                        { R.startThreadName = "Worduel Guess Thread"
                                                        , R.startThreadAutoArchive = Just 60 -- 60 minutes
                                                        , R.startThreadRateLimit = Nothing }
                                                return ()
                                            Left err -> echo $ "Failed to send message to channel " <> showT cid <> ", " <> showT err

                                        -- Create the 6 rows required by the Worduel game, these will be edited as the rounds progress
                                        sendInitialRowMsg cid
                                        sendInitialRowMsg cid
                                        sendInitialRowMsg cid
                                        sendInitialRowMsg cid
                                        sendInitialRowMsg cid
                                        sendInitialRowMsg cid

                                        echo ""
                                        --echo $ showT (length rowMsgs)

                                    Nothing -> echo "Failed to get channel ID of /challenge command, may have been sent in DM"

                                echo "Created new game"

                            Nothing -> echo "Failed to create a new game, no guild ID provided"
            Nothing -> echo "Couldn't get ID of user"
    }

sendInitialRowMsg :: ChannelId -> DiscordHandler (Maybe StoredMessage)
sendInitialRowMsg cid = do
    msgResult <- restCall $ R.CreateMessage cid ":blue_square: :blue_square: :blue_square: :blue_square: :blue_square:          :black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:          :black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:"
    case msgResult of
        Right msg -> return $ Just (StoredMessage cid (messageId msg))
        Left err  -> do
            echo $ "Failed to send message to channel " <> showT cid <> ", " <> showT err
            return Nothing

onDiscordEvent :: IORef GlobalState -> GuildId -> Event -> DiscordHandler ()
onDiscordEvent globalState guildId event = case event of
    Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId guildId
    InteractionCreate intr                         -> onInteractionCreate intr globalState
    _                                              -> return ()

onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady appId guildId = do
    echo "Bot ready!"

    appCmdRegistrations <- mapM tryRegistering mySlashCommands

    case sequence appCmdRegistrations of
        Left err ->
            echo $ "Failed to register some commands: " <> showT err
        Right cmds -> do
            echo $ "Registered " <> showT (length cmds) <> " command(s)"
            unregisterOutdatedCommands cmds

    return ()

    where
        tryRegistering cmd = case registration cmd of
            Just reg -> restCall $ R.CreateGuildApplicationCommand appId guildId reg
            Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

        unregisterOutdatedCommands validCmds = do
            registered <- restCall $ R.GetGuildApplicationCommands appId guildId
            case registered of
                Left err ->
                    echo $ "Failed to get registered slash commands: " <> showT err
                Right cmds ->
                    let validIds = map applicationCommandId validCmds
                        outdatedIds = filter (`notElem` validIds) . map applicationCommandId $ cmds
                    in
                        forM_ outdatedIds $ restCall . R.DeleteGuildApplicationCommand appId guildId

onInteractionCreate :: Interaction -> IORef GlobalState -> DiscordHandler ()
onInteractionCreate intr globalState = case intr of
    cmd@InteractionApplicationCommand { applicationCommandData = input } -> -- @ApplicationCommandDataChatInput {} } ->
        case find (\c -> applicationCommandDataName input == name c) mySlashCommands of
            Just found -> handler found cmd globalState (optionsData input)
            Nothing    -> echo "Unknown slash command (registrations out of date?)"
    _ -> return ()
