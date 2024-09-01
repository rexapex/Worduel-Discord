{-# LANGUAGE OverloadedStrings #-}

module Game (GameId, UserGame(..), StoredMessage(..), Player(..), Game(..), createGame, processPlayerAction, getOpponent, getPlayerByIndex, getPlayerById, getCurrentTurnMsg, genColourHints) where

import Data.List (findIndex, delete)
import qualified Data.Text
import Data.Text (Text, unpack)

import Discord.Types

import WordList (dictionary)

newtype GameId = GameId UserId
    deriving (Eq, Show)

data UserGame = UserGame UserId GameId

data StoredMessage = StoredMessage
    { storedChannelId :: ChannelId
    , storedMessageId :: MessageId
    } deriving Show

data Player = Player
    { playerUserId :: UserId
    , playerHiddenWord :: Text
    } deriving (Eq, Show)

data Game = Game
    { gameId :: GameId
    , gameGuessChannelId :: Maybe ChannelId
    , gameChallengerId :: UserId
    , gameChallengeMsg :: StoredMessage
    , gamePlayers :: [Player]
    , gameCurrentPlayer :: Int
    , gameCurrentTurn :: Int
    , gameGuesses :: [StoredMessage]
    } deriving Show

createGame :: Player -> Player -> StoredMessage -> Game
createGame player1 player2 challengeMsg = Game
    { gameId = GameId $ playerUserId player1 -- TODO - Change this if players can participate in > 1 game at once
    , gameGuessChannelId = Nothing
    , gameChallengerId = playerUserId player1
    , gameChallengeMsg = challengeMsg
    , gamePlayers = [player1, player2]
    , gameCurrentPlayer = 0
    , gameCurrentTurn = -1
    , gameGuesses = [] }

processPlayerAction :: Game -> ChannelId -> UserId -> Text -> Maybe Game
processPlayerAction game cid uid guess = do
    i <- activePlayerIndex
    if i == currentPlayer then do
        guessChannelId <- gameGuessChannelId
        if guessChannelId == cid && Data.Text.length guess == 5 && elem guess dictionary then
            Just Game
               { gameId = gameId
               , gameGuessChannelId = gameGuessChannelId
               , gameChallengerId = gameChallengerId
               , gameChallengeMsg = gameChallengeMsg
               , gamePlayers = ps
               , gameCurrentPlayer = getNextPlayerIndex game
               , gameCurrentTurn = currentTurn + 1
               , gameGuesses = guesses }
        else Nothing
    else Nothing
    where
        Game
            { gameId = gameId
            , gameGuessChannelId = gameGuessChannelId
            , gameChallengerId = gameChallengerId
            , gameChallengeMsg = gameChallengeMsg
            , gamePlayers = ps
            , gameCurrentPlayer = currentPlayer
            , gameCurrentTurn = currentTurn
            , gameGuesses = guesses } = game
        activePlayerIndex = findIndex (\p -> uid == playerUserId p) ps

getOpponent :: Game -> Maybe Player
getOpponent g = getPlayerByIndex g (getNextPlayerIndex g)

getPlayerByIndex :: Game -> Int -> Maybe Player
getPlayerByIndex Game { gamePlayers = players } playerIndex = getFromList players playerIndex 0
    where
        getFromList [] _ _ = Nothing
        getFromList (p : ps) c i
            | i == c    = Just p
            | otherwise = getFromList ps c (i + 1)

getPlayerById :: Game -> UserId -> Maybe Player
getPlayerById Game { gamePlayers = players } = getFromList players
    where
        getFromList [] _ = Nothing
        getFromList (p : ps) pid
            | pid == uid = Just p
            | otherwise  = getFromList ps pid
            where
                Player { Game.playerUserId = uid } = p

getNextPlayerIndex :: Game -> Int
getNextPlayerIndex Game { gamePlayers = players, gameCurrentPlayer = currentPlayer } = (currentPlayer + 1) `mod` length players

getCurrentTurnMsg :: Game -> Maybe StoredMessage
getCurrentTurnMsg Game { gameCurrentTurn = currentTurn, gameGuesses = guesses } = getFromList guesses 0
    where
        getFromList [] _          = Nothing
        getFromList (m : ms) turn
            | turn == currentTurn = Just m
            | otherwise           = getFromList ms (turn + 1)

genColourHints :: Game -> Text -> Text
genColourHints Game { gamePlayers = players } txt = genHintsForPlayers players
    where
        genHintsForPlayers :: [Player] ->  Text
        genHintsForPlayers []     = ""
        genHintsForPlayers (p:ps) = do
            let txtString = unpack txt
            let (statuses, unused) = genGreens txtString (unpack $ playerHiddenWord p) [] []
            let statuses2 = genYellows txtString unused statuses []
            genHintsForPlayer statuses2 <> "         " <> genHintsForPlayers ps

        -- Compare the guess to the hidden word
        -- If the letters match at the same index, add a 1 to the 1st returned list
        -- If the letters don't match, add a 0 to the 1st returned list & add the letter to the 2nd
        genGreens :: String -> String -> [Int] -> [Char] -> ([Int], [Char])
        genGreens [] _ statuses unused = (statuses, unused)
        genGreens _ [] statuses unused = (statuses, unused)
        genGreens (t:ts) (h:hs) statuses unused
            | t == h    = genGreens ts hs (statuses ++ [1]) unused
            | otherwise = genGreens ts hs (statuses ++ [0]) (h : unused)

        -- Take a status list with greens fixed & replace reds with yellows where appropriate
        -- Returns a new status list with reds, greens, and yellows
        genYellows :: String -> [Char] -> [Int] -> [Int] -> [Int]
        genYellows [] _ oldStatuses newStatuses = newStatuses ++ oldStatuses
        genYellows _ [] oldStatuses newStatuses = newStatuses ++ oldStatuses
        genYellows _ _ [] newStatuses = newStatuses
        genYellows (t:ts) unused (s:ss) newStatuses
            | s == 0 && elem t unused = genYellows ts (delete t unused) ss (newStatuses ++ [2])
            | otherwise               = genYellows ts unused ss (newStatuses ++ [s])

        -- Generate a hints string from a status list
        genHintsForPlayer :: [Int] -> Text
        genHintsForPlayer [] = ""
        genHintsForPlayer (s:ss)
            | s == 0    = ":red_square: "    <> genHintsForPlayer ss
            | s == 1    = ":green_square: "  <> genHintsForPlayer ss
            | s == 2    = ":yellow_square: " <> genHintsForPlayer ss
            | otherwise = ":pirate_flag: "   <> genHintsForPlayer ss
