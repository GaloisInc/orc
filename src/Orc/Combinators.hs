
--------------------------------------------------------------------------------
-- |
-- Module      : Orc Combinators
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency
--

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Orc.Combinators where

import Orc.Monad
import qualified Control.Concurrent.StdInOut as S
import Control.DeepSeq


------------------

-- | Alternate phrasing of @return ()@, which can be placed at the end
-- of an Orc computation to signal that it has no more values to
-- produce.
signal :: Orc ()
signal = return ()

------------------

-- | Cut executes an orc expression, waits for the first result, and then
-- suppresses the rest, including killing any threads involved
-- in computing the remainder. 
cut :: Orc a -> Orc a
cut = join . eagerly

-- | Executes the computation @p@ and @done@.  Once @done@ returns its
-- first result, kill both computations and returns that result.  This
-- discards the results of @p@.
onlyUntil :: Orc a -> Orc b -> Orc b
p `onlyUntil` done = cut (silent p <|> done)

-- | Immediately executes the computation @p@, but if it hasn't returned
-- a result in @t@ seconds, execute the computation @q@ and return
-- whichever computations returns a result first (killing the other
-- thread).
butAfter :: Orc a -> (Float, Orc a) -> Orc a
p `butAfter` (t,def) = cut (p <|> (delay t >> def))

-- | Executes a computation @p@, but if it hasn't returned a result in
-- @n@ seconds return @a@ instead (killing the @p@ computation).
timeout :: Float -> a -> Orc a -> Orc a
timeout n a p = cut (p <|> (delay n >> return a))

-- | Executes the computation @p@ but suppresses its results.
silent :: Orc a -> Orc b
silent p = p >> stop

-- | Lifts a list into an Orc monad.
liftList :: (MonadPlus list) => [a] -> list a
liftList ps = foldr mplus mzero $ map return ps

-- | Repeatedly executes the computation @p@ and returns its
-- results.  'repeating' works best when @p@ is single-valued:
-- if @p@ is multivalued Orc will spawn a repeating thread for every
-- result returned, resulting in an exponential blow-up of
-- threads (XXX: I don't think this was actually intended.)
repeating :: Orc a -> Orc a
repeating p = do
    x <- p
    return x <|> repeating p

-- | Runs a computation @p@ and writes its results to the channel @ch@.
runChan :: Chan a -> Orc a -> IO ()
runChan ch p = runOrc $ (p >>= writeChan ch)

--------------------

-- | Takes the first result of @p@, the first result of
-- @q@, and applies them to @f@.  The computations for @p@ and @q@ are
-- run in parallel.
sync :: (a->b->c) -> Orc a -> Orc b -> Orc c
sync f p q = do
  po <- eagerly p
  qo <- eagerly q
  pure f <*> po <*> qo

-- | Runs the computation @p@ and returns its first result, but doesn't
-- return before @w@ seconds have elapsed.
notBefore:: Orc a -> Float -> Orc a
p `notBefore` w = sync const p (delay w)

-- | Runs a list of Orc computations @ps@ in parallel until they produce
-- their first result, and returns a list of all these results.
syncList :: [Orc a] -> Orc [a]
syncList ps = sequence (map eagerly ps) >>= sequence


---------------------------------------------------------------------------

-- | Wait for a period of w seconds, then continue processing.
delay :: (RealFrac a, Show a) => a -> Orc ()
delay w =  (liftIO $ threadDelay (round (w * 1000000)))
       <|> (silent $ do
             guard (w>100)
             putStrLine ("Just checking you meant to wait "
                           ++show w++" seconds"))

---------------------------------------------------------------------------
-- 'printOrc' and 'prompt' uses the 'Stdinout' library to provide
-- basic console input/output in a concurrent setting. 'runOrc' executes
-- an orc expression and prints out the answers eagerly per line.

-- | Runs an Orc computation, eagerly printing out the results of an Orc
-- computation line-by-line.
printOrc :: Show a => Orc a -> IO ()
printOrc p = S.setupStdInOut $ runOrc $ do
    a <- p
    putStrLine ("Ans = " ++ show a)

-- | Prompts the user for a string.  Concurrency-safe.
prompt :: String -> Orc String
prompt str = liftIO $ S.prompt str

-- | Writes a string and newline to standard output.  Concurrency-safe.
putStrLine :: String -> Orc ()
putStrLine str = liftIO $ S.putStrLine str


---------------------------------------------------------------------------

-- | Analogous to the list scan function, but the order in which
-- the combining function is applied to the results produced by
-- @p@ is nondeterministic.
scan :: (a -> s -> s) -> s -> Orc a -> Orc s
scan f s p = do
  accum <- newTVar s
  x <- p
  (_w,w') <- modifyTVar accum (f x)
  return w'

-- | A variant of '<+>', pronounced or-else, which performs and returns
-- the results of @p@, and if @p@ produced no answers go on and performa
-- dn return the results of @q@.
(<?>) :: Orc a -> Orc a -> Orc a
p <?> q = do
    tripwire <- newEmptyMVar
    do x <- p
       tryPutMVar tripwire ()
       return x
     <+>
     do triggered <- tryTakeMVar tripwire
        case triggered of
          Nothing -> q
          Just _  -> stop

-- | For each value produced by @p@, return a @Left a@.  Once @p@ has
-- finished, return a @Right Int@ containing the number of results
-- produced.
count :: Orc a -> Orc (Either a Int)
count p = do
    accum <- newTVar 0
    do x <- p
       modifyTVar accum (+1)
       return $ Left x
     <+>
       fmap Right (readTVar accum)

-- | Collects all of the values of the computation @p@ and delivers them
-- as a list when @p@ is completed.
collect :: Orc a -> Orc [a]
collect p = do
    accum <- newTVar []
    silent (do x <- p
               modifyTVar accum (x:))
     <+>
       readTVar accum


---------------------------------------------------------------------------
-- | List-like functions

-- | Runs the computation @p@ and returns the first @n@ results.
takeOrc :: Int -> Orc a -> Orc a
takeOrc n p = do
    vals <- newEmptyMVar
    end  <- newEmptyMVar
    echo n vals end <|> silent (sandbox p vals end)

-- | Drops the first @n@ results of the computation @p@, and then
-- returns the rest of the results.
dropOrc :: Int -> Orc a -> Orc a
dropOrc n p = do
    countdown <- newTVar n
    x <- p
    join $ atomically $ do 
      w <- readTVarSTM countdown
      if w==0 then return $ return x
        else do
          writeTVarSTM countdown (w-1)
          return stop

-- | Zips the results of two computations @p@ and @q@.  When one
-- computation finishes, kill the other.
zipOrc :: Orc a -> Orc b -> Orc (a,b)
zipOrc p q = do
    pvals <- newEmptyMVar
    qvals <- newEmptyMVar
    end   <- newEmptyMVar
    zipp pvals qvals end
      <|> silent (sandbox p pvals end)
      <|> silent (sandbox q qvals end)

--------------
-- Auxilliary definitions

-- | Runs the computation @p@, and repeatedly puts its results (tagged
-- with 'Just' into the @vals@ 'MVar'.  Puts 'Nothing' if there are no
-- results left.  Stops executing when the @end@ MVar is filled.
sandbox :: Orc a -> MVar (Maybe a) -> MVar () -> Orc ()
sandbox p vals end
  = ((p >>= (putMVar vals . Just)) <+> putMVar vals Nothing)
    `onlyUntil` takeMVar end 

-- | The rough inverse of 'sandbox', repeatedly reads values from the
-- @vals@ 'MVar' until @j@ values have been read or the @vals@ MVar is
-- exhausted (a 'Nothing' is passed).  When there are no more values to
-- be returned, fills the @end@ MVar.
echo :: Int -> MVar (Maybe a) -> MVar () -> Orc a
echo 0  _   end = silent (putMVar end ())
echo j vals end = do
    mx <- takeMVar vals
    case mx of
      Nothing -> silent (putMVar end ())
      Just x  -> return x <|> echo (j-1) vals end

-- | Like 'echo', repeatedly reads values from the @pvals@ and @qvals@
-- 'MVar', returning tuples of the values until one 'MVar' is exhausted.
-- When there are no more values to be returned, fills the @end@ MVar.
zipp :: MVar (Maybe a) -> MVar (Maybe b) -> MVar () -> Orc (a,b)
zipp pvals qvals end = do
    mx <- takeMVar pvals
    my <- takeMVar qvals
    case mx of
      Nothing -> silent (putMVar end () >> putMVar end ())
      Just x  -> case my of
        Nothing -> silent (putMVar end () >> putMVar end ())
        Just y  -> return (x,y) <|> zipp pvals qvals end



---------------------------------------------------------------------------

-- | Publish is a hyperstrict form of return. It is useful
--   for combining results from multiple 'val' computations, providing
--   a synchronization point. 
publish :: NFData a => a -> Orc a
publish x = deepseq x $ return x

