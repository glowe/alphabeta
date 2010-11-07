{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

module TicTacToe (runGame) where

import Control.Exception
import Data.Maybe
import AlphaBeta

data Value = X | O
    deriving (Eq, Read, Show)

pretty :: Square -> (Maybe Value) -> String
pretty square (Just X) = "X"
pretty square (Just O) = "O"
pretty square Nothing  = show square

data Board = Board (Maybe Value, Maybe Value, Maybe Value,
                    Maybe Value, Maybe Value, Maybe Value,
                    Maybe Value, Maybe Value, Maybe Value)
    deriving Show

emptyBoard :: Board
emptyBoard = Board (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

prettyBoard :: Board -> String
prettyBoard (Board (tl, tc, tr, ml, mc, mr, bl, bc, br)) =
    pretty 1 tl ++ "|" ++ pretty 2 tc ++ "|" ++ pretty 3 tr ++ "\n" ++
    "-+-+-\n" ++
    pretty 4 ml ++ "|" ++ pretty 5 mc ++ "|" ++ pretty 6 mr ++ "\n" ++
    "-+-+-\n" ++
    pretty 7 bl ++ "|" ++ pretty 8 bc ++ "|" ++ pretty 9 br

type Square = Int

play :: Board -> Square -> Value -> Maybe Board
play (Board (Nothing, tc, tr,
             ml, mc, mr,
             bl, bc, br)) 1 X = Just $ Board (Just X, tc, tr, ml, mc, mr, bl, bc, br)
play (Board (Nothing, tc, tr,
             ml, mc, mr,
             bl, bc, br)) 1 O = Just $ Board (Just O, tc, tr, ml, mc, mr, bl, bc, br)
play (Board (tl, Nothing, tr,
             ml, mc, mr,
             bl, bc, br)) 2 X = Just $ Board (tl, Just X, tr, ml, mc, mr, bl, bc, br)
play (Board (tl, Nothing, tr,
             ml, mc, mr,
             bl, bc, br)) 2 O = Just $ Board (tl, Just O, tr, ml, mc, mr, bl, bc, br)
play (Board (tl, tc, Nothing,
             ml, mc, mr,
             bl, bc, br)) 3 X = Just $ Board (tl, tc, Just X, ml, mc, mr, bl, bc, br)
play (Board (tl, tc, Nothing,
             ml, mc, mr,
             bl, bc, br)) 3 O = Just $ Board (tl, tc, Just O, ml, mc, mr, bl, bc, br)
play (Board (tl, tc, tr,
             Nothing, mc, mr,
             bl, bc, br)) 4 X = Just $ Board (tl, tc, tr, Just X, mc, mr, bl, bc, br)
play (Board (tl, tc, tr,
             Nothing, mc, mr,
             bl, bc, br)) 4 O = Just $ Board (tl, tc, tr, Just O, mc, mr, bl, bc, br)
play (Board (tl, tc, tr,
             ml, Nothing, mr,
             bl, bc, br)) 5 X = Just $ Board (tl, tc, tr, ml, Just X, mr, bl, bc, br)
play (Board (tl, tc, tr,
             ml, Nothing, mr,
             bl, bc, br)) 5 O = Just $ Board (tl, tc, tr, ml, Just O, mr, bl, bc, br)
play (Board (tl, tc, tr,
             ml, mc, Nothing,
             bl, bc, br)) 6 X = Just $ Board (tl, tc, tr, ml, mc, Just X, bl, bc, br)
play (Board (tl, tc, tr,
             ml, mc, Nothing,
             bl, bc, br)) 6 O = Just $ Board (tl, tc, tr, ml, mc, Just O, bl, bc, br)
play (Board (tl, tc, tr,
             ml, mc, mr,
             Nothing, bc, br)) 7 X = Just $ Board (tl, tc, tr, ml, mc, mr, Just X, bc, br)
play (Board (tl, tc, tr,
             ml, mc, mr,
             Nothing, bc, br)) 7 O = Just $ Board (tl, tc, tr, ml, mc, mr, Just O, bc, br)
play (Board (tl, tc, tr,
             ml, mc, mr,
             bl, Nothing, br)) 8 X = Just $ Board (tl, tc, tr, ml, mc, mr, bl, Just X, br)
play (Board (tl, tc, tr,
             ml, mc, mr,
             bl, Nothing, br)) 8 O = Just $ Board (tl, tc, tr, ml, mc, mr, bl, Just O, br)
play (Board (tl, tc, tr,
             ml, mc, mr,
             bl, bc, Nothing)) 9 X = Just $ Board (tl, tc, tr, ml, mc, mr, bl, bc, Just X)
play (Board (tl, tc, tr,
             ml, mc, mr,
             bl, bc, Nothing)) 9 O = Just $ Board (tl, tc, tr, ml, mc, mr, bl, bc, Just O)
play board _ _ = Nothing -- playing in occupied square

getWinner :: Board -> Maybe Value
getWinner (Board (Just X, Just X, Just X,
                       _,      _,      _,
                       _,      _,      _)) = Just X
getWinner (Board (Just O, Just O, Just O,
                       _,      _,      _,
                       _,      _,      _)) = Just O
getWinner (Board (     _,      _,      _,
                  Just X, Just X, Just X,
                       _,      _,      _)) = Just X
getWinner (Board (     _,      _,      _,
                  Just O, Just O, Just O,
                       _,      _,      _)) = Just O
getWinner (Board (     _,      _,      _,
                       _,      _,      _,
                  Just X, Just X, Just X)) = Just X
getWinner (Board (     _,      _,      _,
                       _,      _,      _,
                  Just O, Just O, Just O)) = Just O
getWinner (Board (Just X,      _,      _,
                  Just X,      _,      _,
                  Just X,      _,      _)) = Just X
getWinner (Board (Just O,      _,      _,
                  Just O,      _,      _,
                  Just O,      _,      _)) = Just O
getWinner (Board (     _, Just X,      _,
                       _, Just X,      _,
                       _, Just X,      _)) = Just X
getWinner (Board (     _, Just O,      _,
                       _, Just O,      _,
                       _, Just O,      _)) = Just O
getWinner (Board (     _,      _, Just X,
                       _,      _, Just X,
                       _,      _, Just X)) = Just X
getWinner (Board (     _,      _, Just O,
                       _,      _, Just O,
                       _,      _, Just O)) = Just O
getWinner (Board (Just X,      _,      _,
                       _, Just X,      _,
                       _,      _, Just X)) = Just X
getWinner (Board (Just O,      _,      _,
                       _, Just O,      _,
                       _,      _, Just O)) = Just O
getWinner (Board (     _,      _, Just X,
                       _, Just X,      _,
                  Just X,      _,      _)) = Just X
getWinner (Board (     _,      _, Just O,
                       _, Just O,      _,
                  Just O,      _,      _)) = Just O
getWinner _                                = Nothing

isWinningBoard :: Board -> Bool
isWinningBoard board =
    case getWinner board of
      Nothing -> False
      Just _ -> True

isEndGame :: Board -> Bool
isEndGame board@(Board (tl, tc, tr, ml, mc, mr, bl, bc, br)) =
    isWinningBoard board || null (emptySquares board)

isEmpty :: Maybe Value -> Bool
isEmpty Nothing = True
isEmpty _     = False

square :: Board -> Square -> Maybe Value
square (Board (tl, _, _, _, _, _, _, _, _)) 1 = tl
square (Board (_, tc, _, _, _, _, _, _, _)) 2 = tc
square (Board (_, _, tr, _, _, _, _, _, _)) 3 = tr
square (Board (_, _, _, ml, _, _, _, _, _)) 4 = ml
square (Board (_, _, _, _, mc, _, _, _, _)) 5 = mc
square (Board (_, _, _, _, _, mr, _, _, _)) 6 = mr
square (Board (_, _, _, _, _, _, bl, _, _)) 7 = bl
square (Board (_, _, _, _, _, _, _, bc, _)) 8 = bc
square (Board (_, _, _, _, _, _, _, _, br)) 9 = br

allSquares :: [Square]
allSquares = [1 .. 9]

emptySquares :: Board -> [Square]
emptySquares board = [s | s <- allSquares, isEmpty $ square board s]

whoseTurn :: Board -> Value
whoseTurn board
    | odd . length $ emptySquares board = X
    | otherwise                         = O

type Move = (Square, Value)

instance MiniMax Board Move where
    isTerminal board                 = isEndGame board
    utility board | getWinner board == Just X = -1
                  | getWinner board == Just O = 1
                  | otherwise                 = 0
    successors board =
        map playMove $ emptySquares board
        where playMove square = ((square, move), (fromJust (play board square move)))
              move = whoseTurn board

anyTry :: IO a -> IO (Either SomeException a)
anyTry = try

readSquare :: String -> IO Square
readSquare prompt =
    do putStrLn prompt
       whereTo <- getLine
       square <- anyTry (evaluate ((read whereTo) :: Square))
       case square of
         Left s  -> readSquare prompt
         Right s -> if 0 < s && s < 10
                       then return s
                       else readSquare prompt

yourMove :: Board -> IO Board
yourMove board =
    do square <- readSquare "Your move (X):"
       let board' = play board square X
       case board' of
         Nothing -> yourMove board
         Just b -> do printBoard b
                      putStr "\n"
                      return b

myMove :: Board -> IO Board
myMove board =
    do putStrLn "My move (O)..."
       let (square, move) = decision board
       let board' = fromJust (play board square move)
       printBoard board'
       putStr "\n"
       return board'

gameLoop :: Board -> IO Board
gameLoop board =
    do if isEndGame board
          then return board
          else do printBoard board
                  board' <- yourMove board
                  if isEndGame board'
                     then return board'
                     else gameLoop =<< myMove board'

printBoard :: Board -> IO ()
printBoard = putStrLn . prettyBoard

runGame :: IO ()
runGame =
    do board <- gameLoop emptyBoard
       putStr "GAME OVER: "
       case getWinner board of
         Just O -> putStrLn "I won!"
         Just X -> putStrLn "You won!"
         _      -> putStrLn "Draw."
