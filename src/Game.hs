--{-# LANGUAGE OverloadedStrings #-}

module Game (GameId, UserGame(..), StoredMessage(..), Player(..), Game(..), createGame, processPlayerAction, getPlayerById, getCurrentTurnMsg) where

import Data.List (findIndex)
import Data.Text (Text)

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

newtype Winner = Winner UserId

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

--getPlayerByIndex :: Game -> Int -> Maybe Player
--getPlayerByIndex  Game { players } playerIndex = getFromList players playerIndex 0
--    where
--        getFromList [] _ _ = Nothing
--        getFromList (p : ps) c i
--            | i == c    = Just p
--            | otherwise = getFromList ps c (i + 1)

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
