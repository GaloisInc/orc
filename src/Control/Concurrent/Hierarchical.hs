--------------------------------------------------------------------------------
-- |
-- Module      : Hierarchical IO Threads
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency, unsafeIntereaveIO
--
-- Hierarchical concurrent threads, which kill all of their descendants
-- when they are killed.

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE CPP #-}


module Control.Concurrent.Hierarchical (

    HIO             -- :: * -> *

  , runHIO          -- :: HIO b -> IO b
  , newPrimGroup
  , newGroup     -- :: HIO Group

  , local
  , close
  , Group
  , finished

  ) where

import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO
import System.IO.Unsafe          -- Global variable for profiling code


---------------------------------------------------------------------------
-- | The 'HIO' monad is simply the 'IO' monad augmented with an
-- environment that tracks the current thread 'Group'.  This permits us
-- to keep track of forked threads and kill them en mass when an
-- ancestor is killed.  The 'HIO' monad is an instance of 'MonadIO', so
-- arbitrary 'IO' actions may be embedded in it; however, the user is
-- advised that any action may be summarily killed, and thus it is of
-- extra importance that appropriate bracketing functions are used.
newtype HIO a = HIO {inGroup :: Group -> IO a}  -- isomorphic to ReaderT Group IO a

instance Functor HIO where
  fmap f (HIO hio) = HIO (fmap (fmap f) hio)

instance Monad HIO where
  return x = HIO $ \_ -> return x
  m >>= k  = HIO $ \w -> do
                 x <- m `inGroup` w
                 k x `inGroup` w


instance Applicative HIO where
  pure  = return
  f <*> x = ap f x

instance MonadIO HIO where
  liftIO io = HIO $ const io


---------------------------------------------------------------------------
-- ^ The thread-registry environment is a hierarchical structure of local
-- thread neighborhoods.

-- | A thread 'Group' keeps tracks of its inhabitants, which may be
-- threads or other 'Group's.
type Group       = (TVar Int, TVar Inhabitants)
-- | A group can be closed, in which case it is empty and cannot accept
-- new inhabitants, or open, in which case it contains any number of
-- threads and groups and may have additional threads/groups registered.
data Inhabitants = Closed | Open [Entry]
data Entry       = Thread ThreadId
                 | Group Group


instance HasFork HIO where
#ifdef __GHC_BLOCK_DEPRECATED__
  fork hio = HIO $ \w -> mask $ \ restore -> do
    when countingThreads incrementThreadCount
    increment w
    fork (do tid <- myThreadId
             register (Thread tid) w
             restore (hio `inGroup` w)
          `finally`
          decrement w)
#else
  fork hio = HIO $ \w -> block $ do
    fork (block (do tid <- myThreadId
                    register (Thread tid) w
                    unblock (hio `inGroup` w))
          `finally`
          decrement w)
#endif


-- | Creates a new thread group and registers the current environment's
-- thread group in it.  If the current group is closed, immediately
-- terminates execution of the current thread.
newGroup :: HIO Group
newGroup = HIO $ \w -> do
    w' <- newPrimGroup
    register (Group w') w
    return w'

-- | Explicitly sets the current 'Group' environment for a 'HIO' monad.
local :: Group -> HIO a -> HIO a 
local w p = liftIO (p `inGroup` w)

-- | Kill all threads which are descendants of a 'Group' and closes the
-- group, disallowing new threads or groups to be added to the group.
-- Doesn't do anything if the group is already closed.
close :: Group -> HIO ()
close (c,t) = liftIO $ fork (kill (Group (c,t)) >> writeTVar c 0)
              >> return ()

-- | Blocks until the 'Group' @w@ is finished executing.
finished :: Group -> HIO ()
finished w = liftIO $ isZero w

-- | Runs a 'HIO' operation inside a new thread group that has no
-- parent, and blocks until all subthreads of the operation are done
-- executing.  If @countingThreads@ is @True@, it then prints some
-- debugging information about the threads run (XXX: this seems
-- suboptimal.)
runHIO :: HIO b -> IO ()
runHIO hio = do
    w <- newPrimGroup
    _r <- hio `inGroup` w
    isZero w
    when countingThreads printThreadReport
    return ()

-- | Creates a new, empty thread group.
newPrimGroup :: IO Group
newPrimGroup = do
  count   <- newTVar 0
  threads <- newTVar (Open [])
  return (count,threads)

-- | Registers a thread/group entry @tid@ in a 'Group', terminating the
-- current thread (suicide) if the group is closed.
register :: Entry -> Group -> IO ()
register tid (_,t) = join $ atomically $ do
  ts <- readTVarSTM t
  case ts of
    Closed    -> return (myThreadId >>= killThread)     -- suicide
    Open tids -> writeTVarSTM t (Open (tid:tids)) >>    -- register
                 return (return ())

-- | Recursively kills a thread/group entry.  Does not do anything the
-- entry is a closed group.
kill :: Entry -> IO ()
kill (Thread tid)  = killThread tid
kill (Group (_,t)) = do
  (ts,_) <- modifyTVar t (const Closed)
  case ts of
    Closed    -> return ()
    Open tids -> sequence_ (map kill tids)

increment, decrement, isZero :: Group -> IO ()
increment (c,_) = modifyTVar_ c (+1)
decrement (c,_) = modifyTVar_ c (\x->x-1)
isZero    (c,_) = atomically $ (readTVarSTM c >>= (check . (==0)))
                      -- block until set (i.e. when locality is finished)


---------------------------------------------------------------------------
--  Profiling code: Records how many threads were created

countingThreads :: Bool
countingThreads = False          -- set to enable reporting or not

threadCount :: TVar Integer
threadCount = unsafePerformIO $ newTVar 0

incrementThreadCount :: IO ()
incrementThreadCount = modifyTVar_ threadCount (+1)

printThreadReport :: IO ()
printThreadReport = do
    n <- readTVar threadCount
    putStrLn "----------------------------"
    putStrLn (show n ++ " HIO threads were forked")

