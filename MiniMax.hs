{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module MiniMax where

import Data.List.Extras.Argmax (argmax)

class MiniMax s a | s -> a where
    isTerminal :: s -> Bool
    utility :: (Num v, Ord v) => s -> v
    successors :: s -> [(a, s)]

decision :: (MiniMax s a) => s -> a
decision state = fst $ argmax (minValue . snd) (successors state)

minValue :: (MiniMax s a, Num v, Ord v) => s -> v
minValue state
    | isTerminal state = utility state
    | otherwise = minimum $ map (maxValue . snd) (successors state)

maxValue :: (MiniMax s a, Num v, Ord v) => s -> v
maxValue state
    | isTerminal state = utility state
    | otherwise = maximum $ map (minValue . snd) (successors state)
