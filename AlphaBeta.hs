{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module AlphaBeta where

import Data.List.Extras.Argmax (argmax)

positiveInfinity :: (Num v, Ord v) => v
positiveInfinity = 9999999999999

negativeInfinity :: (Num v, Ord v) => v
negativeInfinity = -positiveInfinity

class MiniMax s a | s -> a where
    isTerminal :: s -> Bool
    utility :: (Num v, Ord v) => s -> v
    successors :: s -> [(a, s)]

decision :: (MiniMax s a) => s -> a
decision = fst . argmax minValueStart . successors
    where minValueStart (a, s) = minValue s negativeInfinity positiveInfinity

xValue nextValue initialV state alpha beta
       | isTerminal state = utility state
       | otherwise        = nextValue successors' initialV alpha beta
       where successors' = (map snd (successors state))

minValue :: (MiniMax s a, Num v, Ord v) => s -> v -> v -> v
minValue = xValue minValue' positiveInfinity

maxValue :: (MiniMax s a, Num v, Ord v) => s -> v -> v -> v
maxValue = xValue maxValue' negativeInfinity

minValue' :: (MiniMax s a, Num v, Ord v) => [s] -> v -> v -> v -> v
minValue' [] v alpha beta = v
minValue' (s:xs) v alpha beta
    | v' <= alpha = v'  -- Prune
    | otherwise  = minValue' xs v' alpha (min beta v')
    where v' = min v $ maxValue s alpha beta


maxValue' :: (MiniMax s a, Num v, Ord v) => [s] -> v -> v -> v -> v
maxValue' [] v alpha beta = v
maxValue' (s:xs) v alpha beta
    | v' >= beta = v' -- Prune
    | otherwise  = maxValue' xs v' (max alpha v') beta
    where v' = max v $ minValue s alpha beta

