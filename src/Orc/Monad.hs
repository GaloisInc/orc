
--------------------------------------------------------------------------------
-- |
-- Module      : Orc Monad
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency
--
-- Primitive combinators for the Orc EDSL in Haskell.

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


module Orc.Monad (
    Orc              -- :: * -> *   
  , module Control.Monad
  , module Control.Applicative
  , module Control.Concurrent.MonadIO
  , module Control.Concurrent.STM.MonadIO
  , stop             -- :: Orc a
  , eagerly          -- :: Orc a -> Orc (Orc a)
  , val              -- :: Orc a -> Orc a
  , (<+>)            -- :: Orc a -> Orc a -> Orc a
  , runOrc           -- :: Orc a -> IO ()

  ) where


import Control.Monad
import Control.Applicative
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO
import Control.Concurrent.Hierarchical

import System.IO.Unsafe


---------------------------------------------------------------------------

-- | A monad for many-valued concurrency, external actions and managed
-- resources.  An expression of type @Orc a@ may perform many actions
-- and return many results of type @a@.  The 'MonadPlus' instance does
-- not obey the Right-Zero law (@p >> stop /= stop@).
newtype Orc a = Orc {(#) :: (a -> HIO ()) -> HIO ()}

instance Functor Orc where
  fmap f p = Orc $ \k -> p # (k . f)

instance Monad Orc where
  return x = Orc $ \k -> k x
  p >>= h  = Orc $ \k -> p # (\x -> h x # k)
  fail _   = stop

-- | Finish the local thread of operations, so that anything sequenced
-- afterwards is not executed.  It satisfies the following law:
-- @stop >>= k == stop@
stop :: Orc a
stop = Orc $ \_ -> return ()

instance Alternative Orc where
  empty = stop
  (<|>) = par

-- | Parallel choice operator that performs the actions of @p@ and @q@
-- and returns their results as they become available.  Also written
-- as @<|>@. There is no left-right bias: the ordering between @p@ and
-- @q@ is unspecified.  'par' satisfies the following laws (identity,
-- commutativity, associativity and left-distributivity across bind):
--
-- > p <|> stop == p
-- > p <|> q == q <|> p
-- > p <|> (q <|> r) == (p <|> q) <|> r
-- > ((p <|> q) >>= k) == ((p >>= k) <|> (q >>= k))
par :: Orc a -> Orc a -> Orc a
par p q = Orc $ \k -> do
    fork (p # k)
    q # k
{- Fully symmetric version: relevant if using async exceptions etc.
    fork (q # k)
    return ()
-}


instance MonadIO Orc where
  liftIO io = Orc (liftIO io >>=)

-- | Runs an Orc computation, discarding the (many) results of the
-- computation.  See @collect@ on a mechanism for collecting the results
-- of a computation into a list, which may then be passed to another IO
-- thread.
runOrc :: Orc a -> IO ()
runOrc p = runHIO (p # \_ -> return ())

instance Applicative Orc where
  pure    = return
  f <*> x = ap f x

instance MonadPlus Orc where
  mzero = empty
  mplus = (<|>)


---------------------------------------------------------------------------

-- | Biased choice operator (pronounced and-then) that performs the
-- action (and returns all the results) of @p@ first, and then once done
-- performs the action of @q@.
(<+>) :: Orc a -> Orc a -> Orc a
p <+> q = Orc $ \k -> do
    w <- newGroup
    local w $ fork (p # k)
    finished w
    q # k

-- | Immediately fires up a thread for @p@, and then returns a handle to
-- the first result of that thread which is also of type @Orc a@.  An
-- invocation to 'eagerly' is non-blocking, while an invocation of the
-- resulting handle is blocking.  'eagerly' satisfies the following
-- laws:
--
-- Par-eagerly:
--
-- > eagerly p >>= (\x -> k x <|> h)
-- > == (eagerly p >>= k) <|> h
--
-- Eagerly-swap:
--
-- > do y <- eagerly p
-- >    x <- eagerly q
-- >    k x y
-- > == do x <- eagerly q
-- >       y <- eagerly p
-- >       k x y
--
-- Eagerly-IO:
--
-- > eagerly (liftIO m) >> p == (liftIO m >> stop) <|> p
eagerly :: Orc a -> Orc (Orc a)
eagerly p = Orc $ \k -> do
    res <- newEmptyMVar
    w <- newGroup
    local w $ fork (p `saveOnce` (res,w))
    k (liftIO $ readMVar res)

-- | An alternate mechanism for 'eagerly', it fires up a thread for @p@
-- and returns a lazy thunk that contains the single (trimmed) result
-- of the computation.  Be careful to use this function with 'publish'
-- when these lazy values need to be fully evaluated before proceeding
-- further.  For example, the following code succeeds immediately:
--
-- > do x <- val p
-- >    return x
--
-- Whereas this code waits until @p@ has generated one result before
-- returning:
--
-- > do x <- val p
-- >    publish p
val :: Orc a -> Orc a
val p = Orc $ \k -> do
    res <- newEmptyMVar
    w <- newGroup
    local w $ fork (p `saveOnce` (res,w))
    k (unsafePerformIO $ readMVar res)      -- Like unsafeInterleaveIO

saveOnce :: Orc a -> (MVar a,Group) -> HIO ()
p `saveOnce` (r,w) = do 
    ticket <- newMVar ()
    p # \x -> (takeMVar ticket >> putMVar r x >> close w)


---------------------------------------------------------------------------
