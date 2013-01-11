--------------------------------------------------------------------------------
-- |
-- Module      : test
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

module Main (

    module Orc
  , main

  ) where

import Orc

import Control.DeepSeq
import System.Random(randomIO)
import Data.List(sortBy)

main = do
    putStrLn "Enter a number 1-9 to run that example:"
    ans <- getLine
    putStrLn "----------------------------"
    case ans of
      "1" -> printOrc baseball 
      "2" -> printOrc (collect patterns)
      "3" -> printOrc firstmetro
      "4" -> printOrc parvalueVal
      "5" -> printOrc $ count queens
      "6" -> printOrc numbers 
      "7" -> printOrc (scan (flip (++)) [] (takeOrc 10 queens)) 
      "8" -> printOrc $ count (chans [1 .. 12])
      "9" -> printOrc $ takeOrc 100 $ zipOrc (recursive 1) (recursive 1000)
      _   -> putStrLn "Unknown"
    putStrLn "----------------------------"



-------------------------------------
-- Shows a rich and nested set of Orc interactions

baseball :: Orc (String,String)
baseball = do

  team <-  prompt "Name a baseball team" 
               `notBefore` 10
        <|> prompt "Name another team"
               `butAfter` (12, return "Yankees")
        <|> (delay 8 >> return "Mariners")
           
  agree <- prompt ("Do you like "++team++"?")
             `butAfter` (20, guard (team/="Mets") 
                          >> return "maybe")
                
  return (team, agree)


-----------------------------
-- Pattern guards

patterns :: Orc Int
patterns = do
   x <- liftList [1..10]
   y <- liftList [2,4..10]
   (n,True) <- return (x, x `mod` y /= 0)
   return n

------------------
-- Metronome example. Shows recursion. Also demonstrates the killing
-- of the metronome thread when the output is killed with ^C, or
-- after 3 prompts are fulfilled

firstmetro :: Orc String
firstmetro =  return "Respond to 2 prompt within 15 seconds, then check all prompts are dead"
          <|> (delay 1 >> takeOrc 2 (metronome >> prompt "Hello"))
          <|> (delay 15 >> return "15 seconds...")

metronome = signal <|> (delay 2 >> metronome)

------------------
-- Demonstrates the concurency of 'eagerly'

parvalue :: Orc Quote
parvalue = quotes
     (Query "Quote A: Enter a number")
     (Query "Quote B: Enter a number")

newtype Query = Query {text::String} deriving (Eq,Show)
newtype Quote = Quote {price::Int} deriving (Eq,Show,Num,NFData)
noQuote = Quote 0

quotes :: Query -> Query -> Orc Quote
quotes srcA srcB = do
  quoteA <- eagerly $ getQuote srcA
  quoteB <- eagerly $ getQuote srcB
  cut (  (pure least <*> quoteA <*> quoteB)
     <|> (quoteA >>= threshold)
     <|> (quoteB >>= threshold)
     <|> (delay 25 >> quoteA <|> quoteB) 
     <|> (delay 30 >> return noQuote))

least qa qb = if price qa < price qb then qa else qb
threshold q = guard (price q < 300) >> return q

getQuote q = prompt (text q) >>= (return . Quote . read)

parallelOr orc1 orc2 = do
  ox <- eagerly orc1
  oy <- eagerly orc2
  cut (  (ox >>= guard >> return True)
     <|> (oy >>= guard >> return True) 
     <|> (pure (||) <*> ox <*> oy))


parvalueVal :: Orc Quote
parvalueVal = quotesVal
     (Query "Quote A: Enter a number")
     (Query "Quote B: Enter a number")

quotesVal :: Query -> Query -> Orc Quote
quotesVal srcA srcB = do
  quoteA <- val $ getQuote srcA
  quoteB <- val $ getQuote srcB
  cut (  publish (least quoteA quoteB)
     <|> (threshold quoteA)
     <|> (threshold quoteB)
     <|> (delay 25 >> publish quoteA <|> publish quoteB) 
     <|> (delay 30 >> return noQuote))

parallelOrVal orc1 orc2 = do
  x <- val orc1
  y <- val orc2
  cut ( (guard x >> publish True)
    <|> (guard y >> publish True) 
    <|> publish (x || y)   )



------------------
-- N queens

queens = return ("Computing "++show size++"-queens...")
       <+> fmap show (extend [])


size = 13 :: Int

extend :: [Int] -> Orc [Int]
extend xs
  | length xs == 3 = liftList $ extendL xs
  | otherwise      = do
      j <- liftList [1 .. size]
      guard $ not (conflict xs j)
      extend (j:xs)

conflict :: [Int] -> Int -> Bool
conflict rs n
  =  n `elem` rs                          -- column clash
  || n `elem` zipWith (+) rs [1 .. size]  -- diagonal clash
  || n `elem` zipWith (-) rs [1 .. size]  -- other diagonal

extendL :: (MonadPlus liftList) => [Int] -> liftList [Int]
extendL xs
  | length xs == size = return xs
  | otherwise         = do
      j <- liftList [1 .. size]
      guard $ not (conflict xs j)
      extendL (j:xs)

------------------------
-- Shows numbers interpreted as Orc terms

numbers :: Orc String
numbers = tally $ map prompt ["one","two","three","four"]

tally ps = do n <- fmap sum (syncList (map success ps))
              guard  (n/=2)
              return $ show n
           <?>
              return ("You responded to two, you lose... :-)")

success :: Orc String -> Orc Int
success m = (m >> return 1) `butAfter` (10, return 0)


-----------------------
-- Demonstrates lifting functions to complex Orc expressions. 

pairs :: Orc (String, String)
pairs = sync (,) numbers queens

-----------------------
-- Demonstrate that recursion needs to be guarded, otherwise runs away.

recursive :: Int -> Orc Int
recursive n = do
  putStrLine ("Recursion: "++show n)
  liftList [1,-1] <|> (recursive (n+1) >>= \x->return (x+sign(x)))

sign x = if x<0 then -1 else 1

powerset :: [a] -> Orc [a]
powerset xs = liftList $ filterM (const $ liftList [False, True]) xs

threeset :: [a] -> Orc [a]
threeset xs = do
  ys <- powerset xs
  guard (length ys == 3)
  return ys

hassle = (metronome >> putStrLine("Simon"))
            `onlyUntil` (delay 15 >> signal)

chans :: [a] -> Orc [a]
chans xs = do
     ch <- newChan
     readChans 4000 ch <|>
       silent (count (powerset xs) >>= writeChan ch)

readChans 0 ch = do
  ch' <- dupChan ch
  readChans' ch <|> readChans' ch'
readChans n ch = do
  mxs <- readChan ch
  case mxs of
    Left xs -> readChans (n-1) ch
    Right n -> stop

readChans' ch = do
  mxs <- readChan ch
  case mxs of
    Left xs -> return xs <|> readChans' ch
    Right n -> stop

------------------------------

shuffle :: [a] -> IO [a]
shuffle xs = do
    ks <- sequence $ take (length xs) $ repeat (randomIO :: IO Int)
    return $ map snd $ sortBy key $ zip ks xs
  where
    key (a,x) (b,y) = if a<b then LT else GT


