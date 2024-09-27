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
            genHintsForPlayer statuses2 txtString <> "         " <> genHintsForPlayers ps

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
        genHintsForPlayer :: [Int] -> String -> Text
        genHintsForPlayer [] _ = ""
        genHintsForPlayer (s:ss) (t:ts)
            | s == 0    = getRedSquare t    <> " " <> genHintsForPlayer ss ts
            | s == 1    = getGreenSquare t  <> " " <> genHintsForPlayer ss ts
            | s == 2    = getYellowSquare t <> " " <> genHintsForPlayer ss ts
            | otherwise = ":pirate_flag: "         <> genHintsForPlayer ss ts

        getGreenSquare :: Char -> Text
        getGreenSquare 'a' = "<:a_green:1287445425137254564>"
        getGreenSquare 'b' = "<:b_green:1287445435249590323>"
        getGreenSquare 'c' = "<:c_green:1287445442900267059>"
        getGreenSquare 'd' = "<:d_green:1287445452190650368>"
        getGreenSquare 'e' = "<:e_green:1287471706872479916>"
        getGreenSquare 'f' = "<:f_green:1287471716318187590>"
        getGreenSquare 'g' = "<:g_green:1287471724446613639>"
        getGreenSquare 'h' = "<:h_green:1287471731325272135>"
        getGreenSquare 'i' = "<:i_green:1287471738862702715>"
        getGreenSquare 'j' = "<:j_green:1287471749109252147>"
        getGreenSquare 'k' = "<:k_green:1287471757577687203>"
        getGreenSquare 'l' = "<:l_green:1287471764192100403>"
        getGreenSquare 'm' = "<:m_green:1287471771901104209>"
        getGreenSquare 'n' = "<:n_green:1287471778637021255>"
        getGreenSquare 'o' = "<:o_green:1287471785532723250>"
        getGreenSquare 'p' = "<:p_green:1287471792897917069>"
        getGreenSquare 'q' = "<:q_green:1287471799138914358>"
        getGreenSquare 'r' = "<:r_green:1287471806311170078>"
        getGreenSquare 's' = "<:s_green:1287471813915312259>"
        getGreenSquare 't' = "<:t_green:1287471822266306592>"
        getGreenSquare 'u' = "<:u_green:1287471830969614357>"
        getGreenSquare 'v' = "<:v_green:1287471837642752010>"
        getGreenSquare 'w' = "<:w_green:1287471844097654824>"
        getGreenSquare 'x' = "<:x_green:1287471850884042762>"
        getGreenSquare 'y' = "<:y_green:1287471857766764624>"
        getGreenSquare 'z' = "<:z_green:1287471865052397730>"
        getGreenSquare _   = ":green_square:"

        getYellowSquare :: Char -> Text
        getYellowSquare 'a' = "<:a_yellow:1287483034425229352>"
        getYellowSquare 'b' = "<:b_yellow:1287485870651347096>"
        getYellowSquare 'c' = "<:c_yellow:1287485879308386397>"
        getYellowSquare 'd' = "<:d_yellow:1287485886157553786>"
        getYellowSquare 'e' = "<:e_yellow:1287485892893737022>"
        getYellowSquare 'f' = "<:f_yellow:1287485902238650408>"
        getYellowSquare 'g' = "<:g_yellow:1287485909377351722>"
        getYellowSquare 'h' = "<:h_yellow:1287485919078912103>"
        getYellowSquare 'i' = "<:i_yellow:1287485928029425697>"
        getYellowSquare 'j' = "<:j_yellow:1287485934782382191>"
        getYellowSquare 'k' = "<:k_yellow:1287485942399107143>"
        getYellowSquare 'l' = "<:l_yellow:1287485955342729277>"
        getYellowSquare 'm' = "<:m_yellow:1287485963576279163>"
        getYellowSquare 'n' = "<:n_yellow:1287485970215735458>"
        getYellowSquare 'o' = "<:o_yellow:1287485978038108282>"
        getYellowSquare 'p' = "<:p_yellow:1287485985135001634>"
        getYellowSquare 'q' = "<:q_yellow:1287485997143162961>"
        getYellowSquare 'r' = "<:r_yellow:1287486020023222383>"
        getYellowSquare 's' = "<:s_yellow:1287486030269906976>"
        getYellowSquare 't' = "<:t_yellow:1287486038075244564>"
        getYellowSquare 'u' = "<:u_yellow:1287486044693860464>"
        getYellowSquare 'v' = "<:v_yellow:1287486052080292020>"
        getYellowSquare 'w' = "<:w_yellow:1287486058971402270>"
        getYellowSquare 'x' = "<:x_yellow:1287486065430757507>"
        getYellowSquare 'y' = "<:y_yellow:1287486072762269738>"
        getYellowSquare 'z' = "<:z_yellow:1287486079825608736>"
        getYellowSquare _   = ":yellow_square:"

        getRedSquare :: Char -> Text
        getRedSquare 'a' = "<:a_red:1289276482501480549>"
        getRedSquare 'b' = "<:b_red:1289276495176400926>"
        getRedSquare 'c' = "<:c_red:1289276501665120358>"
        getRedSquare 'd' = "<:d_red:1289276508933853278>"
        getRedSquare 'e' = "<:e_red:1289276517767188540>"
        getRedSquare 'f' = "<:f_red:1289276524423548969>"
        getRedSquare 'g' = "<:g_red:1289276531662917742>"
        getRedSquare 'h' = "<:h_red:1289276555381706752>"
        getRedSquare 'i' = "<:i_red:1289276568954474566>"
        getRedSquare 'j' = "<:j_red:1289276575992516699>"
        getRedSquare 'k' = "<:k_red:1289276583588397179>"
        getRedSquare 'l' = "<:l_red:1289276592161427557>"
        getRedSquare 'm' = "<:m_red:1289276599975411712>"
        getRedSquare 'n' = "<:n_red:1289276607906844683>"
        getRedSquare 'o' = "<:o_red:1289276614789566525>"
        getRedSquare 'p' = "<:p_red:1289276623623028817>"
        getRedSquare 'q' = "<:q_red:1289276632002986034>"
        getRedSquare 'r' = "<:r_red:1289276638952947773>"
        getRedSquare 's' = "<:s_red:1289276645953376362>"
        getRedSquare 't' = "<:t_red:1289276653217910814>"
        getRedSquare 'u' = "<:u_red:1289276661212254302>"
        getRedSquare 'v' = "<:v_red:1289276669605056513>"
        getRedSquare 'w' = "<:w_red:1289276677842665544>"
        getRedSquare 'x' = "<:x_red:1289276686793310279>"
        getRedSquare 'y' = "<:y_red:1289276696113184828>"
        getRedSquare 'z' = "<:z_red:1289276703255957577>"
        getRedSquare _   = ":red_square:"
