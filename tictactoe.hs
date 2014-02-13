module TicTacToe where

import Control.Arrow
import Control.Monad
import Data.Char
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

type Pos = Int

instance Show Board where
    show (Board board) = let
        -- We need to split it into groups of three.
        rows :: [[(Int, Maybe Player)]]
        rows = chunksOf 3 (zip [1..] board)
        showPlayer player = " "++show player++" "
        showPos (x, p) = maybe (" "++show x++" ") showPlayer p
        -- Then for each one we need to show the player or nothing
        players = map (intercalate "|" . map showPos) rows
        shownRows = intersperse "-----------" players
        shownBoard = unlines shownRows
        in shownBoard

opponent :: Player -> Player
opponent X = O
opponent O = X

wins :: [(Pos, Pos, Pos)]
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

availableMoves :: Player -> Board -> [(Pos, Board)]
availableMoves player board = map (second zipperToBoard) validMoves
    where
        zipper = boardToZipper board
        allPos = zip [1..] . take 9 $ iterate zipLeft zipper
        available = filter isAvailable allPos
        validMoves = map (second $ update player) available
        isAvailable (_, BoardZipper _ Nothing  _) = True
        isAvailable (_, BoardZipper _ (Just _) _) = False

gameOver :: Board -> Maybe Player
gameOver (Board board)
    | xWon      = Just X
    | oWon      = Just O
    | otherwise = Nothing
    where
        numbered :: [(Maybe Player, Pos)]
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
minimaxDecision p b
    | pos /= 0  = newBoard
    | otherwise = b
    where
        newBoard = putPlayer p b pos
        pos = fst $ maxValue p b

minOrMaxHelper :: (Int -> Int -> Bool)
               -> (Player -> Board -> (Pos, (Int, Board)))
               -> Player
               -> Board
               -> (Pos, (Int, Board))
minOrMaxHelper op f p b
    | terminal b = (0, (utility p b, b))
    | otherwise  = val
        where
            moves :: [(Pos, Board)]
            moves = availableMoves p b
            oppo = opponent p
            vals :: [(Pos, (Int, Board))]
            vals = map (second snd . second (f oppo)) moves
            compareUtil :: (Pos, (Int, Board)) -> (Pos, (Int, Board)) -> (Pos, (Int, Board))
            compareUtil x@(_, (u1, _)) y@(_, (u2, _))
                | u1 `op` u2 = x
                | otherwise  = y
            val :: (Pos, (Int, Board))
            val = foldr1 compareUtil vals

maxValue :: Player -> Board -> (Pos, (Int, Board))
maxValue = minOrMaxHelper (>) minValue

minValue :: Player -> Board -> (Pos, (Int, Board))
minValue = minOrMaxHelper (<) maxValue

putPlayer :: Player -> Board -> Pos -> Board
putPlayer p (Board board) n = Board (replace board n)
    where
        replace (_:bs) 1 = Just p:bs
        replace (b:bs) m = b:replace bs (m - 1)

getPos :: IO Pos
getPos = liftM digitToInt getChar

gameLoop :: IO Board -> IO Board
gameLoop board = do
    b <-board
    print b
    case gameOver b of
        Just p  -> do
            putStrLn $ show p ++ " won!"
            board
        Nothing -> do
            putStrLn "Choose a position"
            pos <- getPos
            putStrLn ""
            let xBoard = putPlayer X b pos
            let newBoard = minimaxDecision O xBoard
            gameLoop $ return newBoard

playGame :: IO Board
playGame = do
    putStrLn "You play as X"
    gameLoop $ return createBoard
