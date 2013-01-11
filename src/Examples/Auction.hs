
--------------------------------------------------------------------------------
-- |
-- Module      : Auction
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency
--

import Orc
import System.Random

data Bidder = Bidder
 { name  :: String
 , logic :: Item -> Price -> Orc Price
 }

type Item  = String
type Price = Int

auction :: Item -> Price -> [Bidder] -> Orc ()
auction item price members = do
  (bid,bidder) <- cut (seekBid item price members)
  continue item bid bidder members

continue :: Item -> Price -> Bidder -> [Bidder] ->
            Orc ()
continue item price bidder members = do
  liftIO $ putStrLn (name bidder++
                     " bids $"++show price)
  mb <- (Just <$> seekBid item price members) 
          `butAfter` (5, return Nothing)
  case mb of
    Nothing -> purchase item price bidder
    Just (bid',bidder') -> 
      continue item bid' bidder' members

seekBid :: Item -> Price -> [Bidder] -> 
           Orc (Price, Bidder)
seekBid item price members 
  = foldr (<|>) stop 
      [consider item price m | m <- members]

consider :: Item -> Price -> Bidder -> 
            Orc (Price, Bidder)
consider item price member = do
  bid <- logic member item price
  guard (bid > price)
  return (bid,member)

purchase :: Item -> Price -> Bidder -> 
            Orc ()
purchase item price bidder = do
  liftIO $ putStrLn (name bidder++" wins "
              ++item++" for $"++show price)



-- Testing ---------------------------------------------------------------------

test = runOrc $ auction "Pink Elephant" 200 [add1,sometimesBidHigh]


rdelay = do
  n <- liftIO $ randomRIO (1,6)
  delay (n::Float)

add1 :: Bidder
add1  = Bidder
  { name  = "add1"
  , logic = \ item price -> do
      rdelay
      return (price + 1)
  }

sometimesBidHigh :: Bidder
sometimesBidHigh  = Bidder
  { name  = "sometimesBidHigh"
  , logic = \ item price -> do
      rdelay
      chance <- liftIO randomIO :: Orc Int
      guard (abs chance `rem` 2 == 0)
      return (price + 10)
  }

