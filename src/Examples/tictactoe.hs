
--------------------------------------------------------------------------------
-- |
-- Module      : TicTacToe
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency
--
-- A test file for the Orc EDSL
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Use the correspondence between a magic square and a tictactoe board
-- 2 9 4
-- 7 5 3
-- 6 1 8
-- A win is any three numbers that add up to 15.

module TicTacToe(
    module Orc
  , Game(..)
  , board
  , example
  , win


  ) where

import Orc
import Data.List

-----------------------

data Game = Game {me,you :: [Int]} deriving Show

board :: [[Int]]
board = [[2, 9, 4],
         [7, 5, 3],
         [6, 1, 8]]
showboard = silent $ liftIO $ putStr $ grid $ 
            map (map show) board

example = Game {me = [7,8], you = [9,3]}
switch (Game xs ys) = Game ys xs
initial = Game [] []

showGame :: Game -> Orc ()
showGame (Game xs ys) 
  = putStrLine $ grid $ map (map (position xs ys)) board

position xs os n
  | n `elem` xs = "X"
  | n `elem` os = "O"
  | True        = " "

grid :: [[String]] -> String
grid [as,bs,cs] = horiz ++ row as ++
                  horiz ++ row bs ++
                  horiz ++ row cs ++
                  horiz ++ "\n"
  where horiz = "+---+---+---+\n"
        row [a,b,c] = "| "++a++" | "++b++" | "++c++" |\n"


getMove :: Game -> IO Game
getMove g@(Game xs ys) = do
  putStr "What's your play? "
  y <- fmap read getLine
  if clash (xs++ys) y 
    then putStr "That position is taken...\n" >> getMove g
    else return (Game xs (y:ys))
  `catch` \_ -> do
     putStr "I didn't understand that. Enter 1..9: "
     getMove g

----------------------

win :: [Int] -> Orc ()
win xs = do
  ys <- powerset xs
  guard (length ys == 3)
  guard (sum ys == 15)

powerset :: [a] -> Orc [a]
powerset xs = filterM (const $ list [False,True]) xs

clash xs x = x `elem` xs

nextMove :: Game -> Orc Game
nextMove g@(Game xs ys) = showGame g 
  >> (do 
    n <- list [1..9]
    guard $ not $ clash (xs++ys) n
    (win (n:xs) >> return (Game (n:xs) ys))
--    <|> (win (n:ys) >> return (Game (n:xs) ys))
  ) <?> (do 
    n <- list [1..9]
    guard $ not $ clash (xs++ys) n
    return (Game (n:xs) ys))


------------

myTurn :: Game -> Orc ()
myTurn g = do
  (Game xs ys) <- cut $ nextMove g
  (win xs >> putStrLine "I win!!")
    <?> (guard (length (xs++ys) == 9) >> putStrLine "It's a draw.")
    <?> yourTurn (Game xs ys)

yourTurn :: Game -> Orc ()
yourTurn g = do
  showGame g
  (Game xs ys) <- liftIO (getMove g)
  (win ys >> putStrLine "You win...")
    <?> (guard (length (xs++ys) == 9) >> putStrLine "It's a draw.")
    <?> myTurn (Game xs ys) 

     
