module TicTacToe where

import Control.Arrow
import Data.List
import Data.List.Split
import Data.Maybe

data Player = O | X
    deriving (Eq, Show)

data Board = Board [Maybe Player]
    deriving (Eq)

data BoardZipper = BoardZipper [Maybe Player] (Maybe Player) [Maybe Player]
    deriving (Eq, Show)

data Utility = Side | Corner | Middle
    deriving (Eq, Show, Enum, Bounded)

instance Show Board where
    show (Board board) = let
        -- We need to split it into groups of three.
        rows = chunksOf 3 board
        showPlayer player = " "++show player++" "
        -- Then for each one we need to show the player or nothing
        players = map (intercalate "|" . map (maybe "   " showPlayer)) rows
        shownRows = intersperse "-----------" players
        shownBoard = unlines shownRows
        in shownBoard

opponent :: Player -> Player
opponent X = O
opponent O = X

wins :: [(Int, Int, Int)]
wins = [ (1,2,3)
       , (1,4,7)
       , (1,5,9)
       , (2,5,8)
       , (3,5,7)
       , (3,6,9)
       , (4,5,6)
       , (7,8,9)
       ]

utility :: Player -> Board -> Int
utility player board = case gameOver board of
    Nothing -> 0
    Just p  -> if p == player then 1 else -1

playOrder :: [Player]
playOrder = [X,O,X,O,X,O,X,O,X]

createBoard :: Board
createBoard = Board $ replicate 9 Nothing

-- | Naive next move. Just goes in order.
naiveMove :: Board -> Player -> Board
naiveMove (Board board) player = Board $ front ++ Just player : back
    where (front, _:back) = break isNothing board

boardToZipper :: Board -> BoardZipper
boardToZipper (Board (b:bs)) = BoardZipper [] b bs

zipLeft :: BoardZipper -> BoardZipper
zipLeft (BoardZipper ls p (r:rs)) = BoardZipper (p:ls) r rs
zipLeft bz                        = bz

zipRight :: BoardZipper -> BoardZipper
zipRight (BoardZipper (l:ls) p rs) = BoardZipper ls l (p:rs)
zipRight bz                        = bz

update :: Player -> BoardZipper -> BoardZipper
update p (BoardZipper ls _ rs) = BoardZipper ls (Just p) rs

zipperToBoard :: BoardZipper -> Board
zipperToBoard (BoardZipper [] p rs)     = Board (p:rs)
zipperToBoard (BoardZipper (l:ls) p rs) = zipperToBoard (BoardZipper ls l (p:rs))

availableMoves :: Player -> Board -> [Board]
availableMoves player board = map zipperToBoard validMoves
    where
        zipper = boardToZipper board
        allPos = take 9 $ iterate zipLeft zipper
        available = filter isAvailable allPos
        validMoves = map (update player) available
        isAvailable (BoardZipper _ Nothing  _) = True
        isAvailable (BoardZipper _ (Just _) _) = False

gameOver :: Board -> Maybe Player
gameOver (Board board)
    | xWon      = Just X
    | oWon      = Just O
    | otherwise = Nothing
    where
        numbered :: [(Maybe Player, Int)]
        numbered = zip board [1..]
        played = map (first fromJust) $ filter (isJust . fst) numbered
        (xs, os) = partition ((== X) . fst) played
        xNums = map snd xs
        oNums = map snd os
        xLines = [(a,b,c) | a <- xNums, b <- xNums, c <- xNums, a < b, b < c]
        oLines = [(a,b,c) | a <- oNums, b <- oNums, c <- oNums, a < b, b < c]
        xWon = any (`elem` wins) xLines
        oWon = any (`elem` wins) oLines

terminal :: Board -> Bool
terminal b@(Board board) = noMoves || won
    where
        noMoves = all isJust board
        won = isJust $ gameOver b

minimaxDecision :: Player -> Board -> Board
minimaxDecision p b = snd $ maxValue p b

maxValue :: Player -> Board -> (Int, Board)
maxValue p b
    | terminal b = (utility p b, b)
    | otherwise  = foldr1 compareValues (map (minValue (opponent p)) (availableMoves p b))

    where
        compareValues (v1, b1) (v2, b2)
            | v1 > v2   = (v1, b1)
            | otherwise = (v2, b2)

minValue :: Player -> Board -> (Int, Board)
minValue p b
    | terminal b = (utility p b, b)
    | otherwise  = foldr1 compareValues (map (maxValue (opponent p)) (availableMoves p b))

    where
        compareValues (v1, b1) (v2, b2)
            | v1 < v2   = (v1, b1)
            | otherwise = (v2, b2)
