--{-# LANGUAGE OverloadedStrings #-}

module Game (GameId, UserGame(..), StoredMessage(..), Player(..), Game(..), newGame, processPlayerAction, getPlayerById) where

--import Data.Text (Text)
--import qualified Data.Text.IO as TIO
--import Control.Monad (forM_, void)
--import Data.List (find)

--import Discord
import Discord.Types
import Data.List (elemIndex)
--import Discord.Interactions
--import qualified Discord.Requests as R

newtype GameId = GameId Int
    deriving Eq

data UserGame = UserGame UserId GameId

data StoredMessage = StoredMessage
    { storedChannelId :: ChannelId
    , storedMessageId :: MessageId
    } deriving Show

data Player = Player
    { playerUserId :: UserId
    }
    deriving Eq

data Game = Game
    { gameId :: GameId
    , guildId :: GuildId
    , players :: [Player]
    , currentPlayer :: Int
    , guesses :: [StoredMessage]
    }

newtype Winner = Winner UserId

newGame :: GuildId -> UserId -> UserId -> Game
newGame gid uid1 uid2 = Game { Game.gameId = GameId 0, Game.guildId = gid, players = [newPlayer uid1, newPlayer uid2], currentPlayer = 0 }
    where
        newPlayer uid = Player { Game.playerUserId = uid }

processPlayerAction :: Game -> Player -> Game
processPlayerAction g p = do
    case activePlayerIndex of
        Just i  -> if i == currentPlayer
                       then Game { Game.guildId = gid, players = ps, currentPlayer = getNextPlayerIndex g }
                       else g
        Nothing -> g
    where
        Game { Game.guildId = gid, players = ps, currentPlayer = currentPlayer } = g
        activePlayerIndex = elemIndex p ps

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
