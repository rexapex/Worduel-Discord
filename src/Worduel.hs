{-# LANGUAGE OverloadedStrings #-}

module Worduel (worduel) where

import Helpers
import Game

import qualified Data.Text
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, void, replicateM)
import Control.Monad.IO.Class
import Data.List (find, deleteBy)
import Data.IORef

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R
import Data.Maybe (catMaybes)
import Discord.Requests (MessageDetailedOpts(MessageDetailedOpts))

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
        GlobalState games userGames <- liftIO $ readIORef globalState
        let MemberOrUser memberOrUser = interactionUser intr
        let senderId = case memberOrUser  of
                           Left m  -> userId <$> memberUser m
                           Right u -> Just $ userId u
        let guildId = interactionGuildId intr

        case senderId of
            Just sid -> do
                let userGame = find (\(UserGame uid _) -> uid == sid) userGames
                case userGame of
                    Just (UserGame _ _) -> echo "Failed to create game, user is already in a game!"
                    Nothing             -> do
                        -- Create a game, create a player, link the player to the game
                        case guildId of
                            Just gid -> do
                                void . restCall $
                                    R.CreateInteractionResponse
                                        (interactionId intr)
                                        (interactionToken intr)
                                        (interactionResponseBasic "Starting game of Worduel!")

                                let channelId = interactionChannelId intr
                                case channelId of
                                    Just cid -> do
                                        -- Create the thread where players will send guesses
                                        threadId <- createGuessThread cid

                                        -- Create the 6 rows required by the Worduel game, these will be edited as the rounds progress
                                        msgs <- catMaybes <$> replicateM 6 (sendInitialRowMsg cid)

                                        -- TODO - Currently using requester ID for both players
                                        let newGame = createGame gid threadId sid sid msgs
                                        let newUserGame = UserGame sid (gameId newGame)

                                        liftIO $ writeIORef globalState (GlobalState (newGame : games) (newUserGame : userGames))

                                    Nothing -> echo "Failed to get channel ID of /challenge command, may have been sent in DM"

                                echo "Created new game"

                            Nothing -> echo "Failed to create a new game, no guild ID provided"
            Nothing -> echo "Couldn't get ID of user"
    }

createGuessThread :: ChannelId -> DiscordHandler ChannelId
createGuessThread cid = do
    startThreadResult <- restCall $
        R.StartThreadNoMessage cid R.StartThreadNoMessageOpts
        { R.startThreadNoMessageBaseOpts = R.StartThreadOpts
            { R.startThreadName = "Worduel Guess Thread"
            , R.startThreadAutoArchive = Just 60 -- 60 minutes
            , R.startThreadRateLimit = Nothing }
        , R.startThreadNoMessageType = 11
        , R.startThreadNoMessageInvitable = Just False }
    case startThreadResult of
        Right thread -> do
            msgResult <- restCall $ R.CreateMessage (channelId thread) "Check DMs for your hidden word & enter guesses in the thread"
            case msgResult of
                Right _  -> return $ channelId thread
                Left err -> echo ("Failed to start thread in channel " <> showT cid <> ", " <> showT err) >> return cid
        Left err -> echo ("Failed to send message to channel " <> showT cid <> ", " <> showT err) >> return cid

sendInitialRowMsg :: ChannelId -> DiscordHandler (Maybe StoredMessage)
sendInitialRowMsg cid = do
    msgResult <- restCall $ R.CreateMessage cid ":blue_square: :blue_square: :blue_square: :blue_square: :blue_square:          :black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:          :black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:"
    case msgResult of
        Right msg -> return $ Just (StoredMessage cid (messageId msg))
        Left err  -> do
            echo $ "Failed to send message to channel " <> showT cid <> ", " <> showT err
            return Nothing

updateRowWithGuess :: StoredMessage -> Text -> DiscordHandler ()
updateRowWithGuess msg txt = do
    let txt2 = Data.Text.concatMap (\c -> ":regional_indicator_" <> Data.Text.singleton c <> ": ") txt
    let txt3 = txt2 <> "         :black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:          :black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:"
    updatedMsg <- editMsg msg txt3
    echo ""

editMsg :: StoredMessage -> Text -> DiscordHandler StoredMessage
editMsg msg txt = do
    msgResult <- restCall $ R.EditMessage (storedChannelId msg, storedMessageId msg) (MessageDetailedOpts
        { R.messageDetailedContent = txt
        , R.messageDetailedTTS = False
        , R.messageDetailedEmbeds = Nothing
        , R.messageDetailedFile = Nothing
        , R.messageDetailedAllowedMentions = Nothing
        , R.messageDetailedReference = Nothing
        , R.messageDetailedComponents = Nothing
        , R.messageDetailedStickerIds = Nothing })
    case msgResult of
        -- TODO - Verify messageId stays the same
        Right _ -> echo $ "Successfully edited message" <> showT msg--StoredMessage { storedChannelId = (storedChannelId)storedMessageId updatedMsg
        Left err -> echo $ "Failed to edit message " <> showT msg <> ", " <> showT err
    return msg

getGameOfUser :: UserId -> IORef GlobalState -> DiscordHandler (Maybe Game)
getGameOfUser uid globalState = do 
    GlobalState games userGames <- liftIO $ readIORef globalState
    let userGame = find (\(UserGame uid2 _) -> uid == uid2) userGames
    return $ case userGame of
        Just (UserGame _ gid) -> find (\Game { gameId = gameId } -> gameId == gid) games
        Nothing               -> Nothing

onDiscordEvent :: IORef GlobalState -> GuildId -> Event -> DiscordHandler ()
onDiscordEvent globalState guildId event = case event of
    Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId guildId
    InteractionCreate intr                         -> onInteractionCreate intr globalState
    MessageCreate msg                              -> onMessageCreate msg globalState
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

onMessageCreate :: Message -> IORef GlobalState -> DiscordHandler ()
onMessageCreate msg globalState = do
    GlobalState games userGames <- liftIO $ readIORef globalState
    let txt = messageContent msg
    --echo $ "onMessageCreate " <> txt <> " " <> showT (Data.Text.length txt)
    if Data.Text.length txt == 5 then do
        --echo "Message is 5 characters long"
        let uid = userId $ messageAuthor msg
        maybeGame <- getGameOfUser uid globalState
        case maybeGame of
            Just game -> do
                --echo "Found game"
                if gameGuessChannelId game == messageChannelId msg then do
                    let maybeStoredMsg = getCurrentTurnMsg game
                    echo $ "Current Turn: " <> showT (currentTurn game)
                    let updatedGame = processPlayerAction game (Player uid)
                    echo $ "Current Turn after Update: " <> showT (currentTurn updatedGame)
                    case maybeStoredMsg  of
                        Just storedMsg -> do
                            updateRowWithGuess storedMsg txt
                            let newGames = updatedGame : deleteBy (\x y -> gameId x == gameId y) game games
                            liftIO $ writeIORef globalState (GlobalState newGames userGames)
                            echo "Found message in thread"
                        Nothing -> echo ""
                    echo ""
                else return ()
            Nothing -> return ()
    else return ()
