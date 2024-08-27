{-# LANGUAGE OverloadedStrings #-}

module Game (GameId, UserGame(..), StoredMessage(..), Player(..), Game(..), createGame, processPlayerAction, getOpponent, getPlayerById, getCurrentTurnMsg, genColourHints) where

import Data.List (findIndex, delete)
import Data.Text (Text, unpack)

import Discord.Types

newtype GameId = GameId Int
    deriving Eq

data UserGame = UserGame UserId GameId

data StoredMessage = StoredMessage
    { storedChannelId :: ChannelId
    , storedMessageId :: MessageId
    } deriving Show

data Player = Player
    { playerUserId :: UserId
    , playerHiddenWord :: Text
    }
    deriving Eq

data Game = Game
    { gameId :: GameId
    , gameGuessChannelId :: ChannelId
    , guildId :: GuildId
    , players :: [Player]
    , currentPlayer :: Int
    , currentTurn :: Int
    , guesses :: [StoredMessage]
    }

createGame :: GuildId -> ChannelId -> Player -> Player -> [StoredMessage] -> Game
createGame gid cid player1 player2 initialMsgs = Game
    { Game.gameId = GameId 0
    , Game.gameGuessChannelId = cid
    , Game.guildId = gid
    , players = [player1]--, newPlayer uid2]
    , currentPlayer = 0
    , currentTurn = 0
    , guesses = initialMsgs }

processPlayerAction :: Game -> UserId -> Game
processPlayerAction g uid = do
    case activePlayerIndex of
        Just i  -> if i == currentPlayer
                       then Game
                           { gameId = gameId
                           , Game.guildId = gid
                           , gameGuessChannelId = gameGuessChannelId
                           , players = ps
                           , currentPlayer = getNextPlayerIndex g
                           , currentTurn = currentTurn + 1
                           , guesses = guesses }
                       else g
        Nothing -> g
    where
        Game
            { gameId = gameId
            , Game.guildId = gid
            , gameGuessChannelId = gameGuessChannelId
            , players = ps
            , currentPlayer = currentPlayer
            , currentTurn = currentTurn
            , guesses = guesses } = g
        activePlayerIndex = findIndex (\p -> uid == playerUserId p) ps

getOpponent :: Game -> Maybe Player
getOpponent g = getPlayerByIndex g (getNextPlayerIndex g)

getPlayerByIndex :: Game -> Int -> Maybe Player
getPlayerByIndex Game { players = players } playerIndex = getFromList players playerIndex 0
    where
        getFromList [] _ _ = Nothing
        getFromList (p : ps) c i
            | i == c    = Just p
            | otherwise = getFromList ps c (i + 1)

getPlayerById :: Game -> UserId -> Maybe Player
getPlayerById Game { players = players } = getFromList players
    where
        getFromList [] _ = Nothing
        getFromList (p : ps) pid
            | pid == uid = Just p
            | otherwise  = getFromList ps pid
            where
                Player { Game.playerUserId = uid } = p

getNextPlayerIndex :: Game -> Int
getNextPlayerIndex Game { players = players, currentPlayer = currentPlayer } = (currentPlayer + 1) `mod` length players

getCurrentTurnMsg :: Game -> Maybe StoredMessage
getCurrentTurnMsg Game { currentTurn = currentTurn, guesses = guesses } = getFromList guesses 0
    where
        getFromList [] _          = Nothing
        getFromList (m : ms) turn
            | turn == currentTurn = Just m
            | otherwise           = getFromList ms (turn + 1)

genColourHints :: Game -> Text -> Text
genColourHints Game { players = players } txt = genHintsForPlayers players
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
