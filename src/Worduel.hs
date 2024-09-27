{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Worduel (worduel) where

import Helpers
import Game

import System.Random (randomRIO)

import qualified Data.Text
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, void, replicateM, when)
import Control.Monad.IO.Class
import Data.List (find, deleteBy)
import Data.IORef
import Data.Foldable (for_)

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R
import Data.Maybe (catMaybes)
import Discord.Requests (MessageDetailedOpts(MessageDetailedOpts))
import WordList (targetWords)
import qualified Discord.Interactions as R

newtype GlobalState = GlobalState  [Game]

worduel :: IO ()
worduel = do
    echo "Worduel!"

    tok <- getToken
    guildId <- getGuildId

    globalState <- newIORef $ GlobalState []

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
mySlashCommands = [challenge]

challenge :: SlashCommand
challenge = SlashCommand
    { name = "challenge"
    , registration = Just R.CreateApplicationCommandChatInput
        { createName = "challenge"
        , createLocalizedName = Nothing
        , createDescription = "Challenge player to a game of Worduel"
        , createLocalizedDescription = Nothing
        , createOptions = Just $ R.OptionsValues
            [ R.OptionValueUser
                { optionValueName = "player"
                , optionValueLocalizedName = Nothing
                , optionValueDescription = "Player to challenge"
                , optionValueLocalizedDescription = Nothing
                , optionValueRequired = True } 
            ]
        , createDefaultMemberPermissions = Nothing
        , createDMPermission = Nothing
        }
    , handler = \intr globalState maybeOptions -> do
        GlobalState games <- liftIO $ readIORef globalState
        let MemberOrUser memberOrUser = interactionUser intr
        let senderId = case memberOrUser  of
                           Left m  -> userId <$> memberUser m
                           Right u -> Just $ userId u
        let guildId = interactionGuildId intr

        case senderId of
            Just sid -> do
                -- TODO - This should also fail if the opponent is in a game
                let maybeGame = getGameOfUser sid games
                case maybeGame of
                    Just _ -> do
                        void . restCall $
                            R.CreateInteractionResponse
                                (interactionId intr)
                                (interactionToken intr)
                                (interactionResponseBasic "You're already in a game!")
                        echo "Failed to create game, user is already in a game!"
                    Nothing             -> do
                        -- Create a game, create a player, link the player to the game
                        case guildId of
                            Just _ -> do
                                createChallengeMsg intr sid maybeOptions globalState

                            Nothing -> do
                                void . restCall $
                                    R.CreateInteractionResponse
                                        (interactionId intr)
                                        (interactionToken intr)
                                        (interactionResponseBasic "Failed to create a new game, only available in a server")
                                echo "Failed to create a new game, no guild ID provided"
            Nothing -> do
                void . restCall $
                    R.CreateInteractionResponse
                        (interactionId intr)
                        (interactionToken intr)
                        (interactionResponseBasic "Failed to create a new game, couldn't get ID of user")
                echo "Failed to create a new game, couldn't get ID of user"
    }

createChallengeMsg :: Interaction -> UserId -> Maybe OptionsData -> IORef GlobalState -> DiscordHandler ()
createChallengeMsg intr senderId maybeOptions globalState = do
    case maybeOptions of
        Just (OptionsDataValues values) -> do
            let maybePlayerOption = find(\case OptionDataValueUser { optionDataValueName = name } -> name == "player"
                                               _                                                  -> False) values
            for_ maybePlayerOption $ \playerOption -> do
                let opponentId = optionDataValueUser playerOption
                maybeBot <- restCall R.GetCurrentUser
                for_ maybeBot $ \bot -> do
                    if senderId /= opponentId && opponentId /= userId bot then do
                        result <- restCall $ R.CreateInteractionResponse
                                (interactionId intr)
                                (interactionToken intr)
                                (interactionResponseBasic $ "<@" <> showT opponentId <> ">, <@" <> showT senderId <> "> challenged you to a game of Worduel");
                        case result of
                            Right _ -> do
                                -- Fetch the original interaction response message
                                msgResult <- restCall $ R.GetOriginalInteractionResponse (interactionApplicationId intr) (interactionToken intr)
                                case msgResult of
                                    Right msg -> do
                                        -- Send a reaction to the message
                                        let msgId = messageId msg
                                        let channelId = messageChannelId msg
                                        sendReaction channelId msgId ":white_check_mark:"
                                        sendReaction channelId msgId ":x:"

                                        let player1 = Player { playerUserId = senderId, playerHiddenWord = "" }
                                        let player2 = Player { playerUserId = opponentId, playerHiddenWord = "" }
                                        let newGame = createGame player1 player2 (StoredMessage channelId msgId)

                                        GlobalState games <- liftIO $ readIORef globalState
                                        liftIO $ writeIORef globalState (GlobalState (newGame : games))

                                    Left err -> echo $ "Failed to fetch original message: " <> showT err
                            Left err -> echo $ "Failed to send challenge msg response: " <> showT err
                    else void . restCall $ R.CreateInteractionResponse
                            (interactionId intr)
                            (interactionToken intr)
                            (interactionResponseBasic "Invalid opponent");

                    
        _ -> echo ""

createGuessThread :: ChannelId -> UserId -> UserId -> DiscordHandler ChannelId
createGuessThread cid playerId1 playerId2 = do
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
            msgResult <- restCall $ R.CreateMessage (channelId thread) ("<@" <> showT playerId1 <> "> (**left**) vs <@" <> showT playerId2 <> "> (**right**)\nCheck DMs for your hidden word, guess your opponent's to win!")
            case msgResult of
                Right _  -> return $ channelId thread
                Left err -> echo ("Failed to start thread in channel " <> showT cid <> ", " <> showT err) >> return cid
        Left err -> echo ("Failed to send message to channel " <> showT cid <> ", " <> showT err) >> return cid

sendInitialRowMsg :: ChannelId -> DiscordHandler (Maybe StoredMessage)
sendInitialRowMsg cid = do
    msgResult <- restCall $ R.CreateMessage cid ":black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:          :black_large_square: :black_large_square: :black_large_square: :black_large_square: :black_large_square:"
    case msgResult of
        Right msg -> return $ Just (StoredMessage cid (messageId msg))
        Left err  -> do
            echo $ "Failed to send message to channel " <> showT cid <> ", " <> showT err
            return Nothing

updateRowWithGuess :: Game -> StoredMessage -> Text -> DiscordHandler ()
updateRowWithGuess game msg txt = do
    _ <- editMsg msg $ genColourHints game txt
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
        Right _ -> echo $ "Successfully edited message" <> showT msg
        Left err -> echo $ "Failed to edit message " <> showT msg <> ", " <> showT err
    return msg

sendDM :: UserId -> Text -> DiscordHandler ()
sendDM uid txt = do
    dmResult <- restCall $ R.CreateDM uid
    case dmResult of
        Right channel -> do
            void . restCall $ R.CreateMessage (channelId channel) txt
        Left err      -> echo $ "Failed to create DM to send msg " <> showT txt <> ", " <> showT err

sendReaction :: ChannelId -> MessageId -> Text -> DiscordHandler ()
sendReaction cid mid txt = do
    void . restCall $ R.CreateReaction (cid, mid) txt

getGameOfUser :: UserId -> [Game] -> Maybe Game
getGameOfUser uid games = do 
    let x :: Game -> Bool
        x g = uid `elem` [ playerUserId p | p <- gamePlayers g ]
    let y :: Maybe Game
        y = find x games
    y

onDiscordEvent :: IORef GlobalState -> GuildId -> Event -> DiscordHandler ()
onDiscordEvent globalState guildId event = case event of
    Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId guildId
    InteractionCreate intr                         -> onInteractionCreate intr globalState
    MessageCreate msg                              -> onMessageCreate msg globalState
    MessageReactionAdd reaction                    -> onMessageReactionAdd reaction globalState
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
    cmd@InteractionApplicationCommand { applicationCommandData = input } ->
        case find (\c -> applicationCommandDataName input == name c) mySlashCommands of
            Just found -> handler found cmd globalState (optionsData input)
            Nothing    -> echo "Unknown slash command (registrations out of date?)"
    _ -> return ()

onMessageCreate :: Message -> IORef GlobalState -> DiscordHandler ()
onMessageCreate msg globalState = do
    GlobalState games <- liftIO $ readIORef globalState
    let txt = messageContent msg
    let uid = userId $ messageAuthor msg
    let maybeGame = getGameOfUser uid games
    for_ maybeGame $ \game -> do
        echo $ "Current Turn: " <> showT game
        let maybeUpdatedGame = processPlayerAction game (messageChannelId msg) uid txt
        case maybeUpdatedGame of
            Just updatedGame -> do
                echo $ "Current Turn after Update: " <> showT updatedGame
                for_ (getCurrentTurnMsg game) $ \storedMsg -> do
                    updateRowWithGuess game storedMsg txt
                    -- First, check if the guess matches the opponent's hidden word
                    if Just txt == (playerHiddenWord <$> getOpponent game) then do
                        let updatedGames = deleteGame game games
                        liftIO $ writeIORef globalState (GlobalState updatedGames)
                        sendReaction (messageChannelId msg) (messageId msg) ":partying_face:"
                        sendWinnerMsg (messageChannelId msg) (messageAuthor msg) " has won the duel!"
                        mapM_ (\p -> sendWordRevealMsg (messageChannelId msg) p) (gamePlayers game)
                        echo "Guesser Wins!"
                    -- Second, check if the guess matches the current player's hidden word
                    else if Just txt == (playerHiddenWord <$> getPlayerById game uid) then do
                        let updatedGames = deleteGame game games
                        liftIO $ writeIORef globalState (GlobalState updatedGames)
                        sendReaction (messageChannelId msg) (messageId msg) ":sob:"
                        sendWinnerMsg (messageChannelId msg) (messageAuthor msg) " guessed their own word!"
                        mapM_ (\p -> sendWordRevealMsg (messageChannelId msg) p) (gamePlayers game)
                        echo "Opponent Wins!"
                    -- Third, check if this was the final turn
                    else if gameCurrentTurn game == 5 then do
                        let updatedGames = deleteGame game games
                        liftIO $ writeIORef globalState (GlobalState updatedGames)
                        sendReaction (messageChannelId msg) (messageId msg) ":thumbsdown:"
                        sendWinnerMsg (messageChannelId msg) (messageAuthor msg) " failed to guess correctly, game ends in a draw!"
                        mapM_ (\p -> sendWordRevealMsg (messageChannelId msg) p) (gamePlayers game)
                        echo "Draw!"
                    -- Else, no player has won so update the game
                    else do
                        let updatedGames = updatedGame : deleteGame game games
                        liftIO $ writeIORef globalState (GlobalState updatedGames)
                        sendReaction (messageChannelId msg) (messageId msg) ":white_check_mark:"
                        case getPlayerByIndex updatedGame (gameCurrentPlayer updatedGame) of
                            Just p -> void . restCall $ R.CreateMessage (messageChannelId msg) ("<@" <> showT (playerUserId p) <> ">'s turn")
                            Nothing -> echo ""
                        echo "No Winner Yet!"
            Nothing -> sendReaction (messageChannelId msg) (messageId msg) ":x:"
    where
        sendWinnerMsg :: ChannelId -> User -> Text -> DiscordHandler ()
        sendWinnerMsg cid winner txt = void . restCall $ R.CreateMessage cid ("<@" <> showT (userId winner) <> ">" <> txt)

        sendWordRevealMsg :: ChannelId -> Player -> DiscordHandler ()
        sendWordRevealMsg cid p = void . restCall $ R.CreateMessage cid ("<@" <> showT (playerUserId p) <> ">'s word was **" <> (playerHiddenWord p) <> "**")

onMessageReactionAdd :: ReactionInfo -> IORef GlobalState -> DiscordHandler ()
onMessageReactionAdd reaction globalState = do
    let msgId = reactionMessageId reaction
    let channelId = reactionChannelId reaction
    let userId = reactionUserId reaction
    let emoji = reactionEmoji reaction

    echo $ emojiName emoji
    if emojiName emoji == "✅" then do
        -- Get the game, get the stored msg, get the channel id
        GlobalState games <- liftIO $ readIORef globalState
        let maybeGame = getGameOfUser userId games
        for_ maybeGame $ \game -> do
            let maybePlayer2 = getPlayerByIndex game 1
            for_ maybePlayer2 $ \player2 -> do
                when (msgId == storedMessageId (gameChallengeMsg game) && (gameCurrentTurn game == -1) && (playerUserId player2 == userId)) $ do
                    let challengerId = gameChallengerId game

                    -- Create the thread where players will send guesses
                    threadId <- createGuessThread channelId challengerId userId

                    -- Create the 6 rows required by the Worduel game, these will be edited as the rounds progress
                    msgs <- catMaybes <$> replicateM 6 (sendInitialRowMsg channelId)

                    -- Generate the hidden words and send them to the players
                    wordIndex1 <- randomRIO (0, length targetWords - 1)
                    wordIndex2 <- randomRIO (0, length targetWords - 1)
                    let word1 = targetWords !! wordIndex1
                    let word2 = targetWords !! wordIndex2
                    sendDM challengerId word1
                    sendDM userId word2

                    let updatedPlayer1 = Player { playerUserId = challengerId, playerHiddenWord = word1 }
                    let updatedPlayer2 = Player { playerUserId = userId, playerHiddenWord = word2 }
                    let updatedGame = Game
                            { gameId = gameId game
                            , gameGuessChannelId = Just threadId
                            , gameChallengerId = challengerId
                            , gameChallengeMsg = gameChallengeMsg game
                            , gamePlayers = [updatedPlayer1, updatedPlayer2]
                            , gameCurrentPlayer = 0
                            , gameCurrentTurn = 0
                            , gameGuesses = msgs }
                    void . restCall $ R.CreateMessage threadId ("<@" <> showT challengerId <> ">'s turn")

                    let updatedGames = updatedGame : deleteGame game games
                    liftIO $ writeIORef globalState (GlobalState updatedGames)
    else when (emojiName emoji == "❌") $ do
        GlobalState games <- liftIO $ readIORef globalState
        let maybeGame = getGameOfUser userId games
        for_ maybeGame $ \game -> do
            when (userId `elem` ([playerUserId p | p <- gamePlayers game])) $ do
                void . restCall $ R.DeleteMessage (channelId, storedMessageId $ gameChallengeMsg game)
                let updatedGames = deleteGame game games
                liftIO $ writeIORef globalState (GlobalState updatedGames)

deleteGame :: Game -> [Game] -> [Game]
deleteGame = deleteBy (\x y -> gameId x == gameId y)
