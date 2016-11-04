{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Graphics.UI.Threepenny.Timer (
    -- * Synopsis
    -- | Implementation of a simple timer which runs on the server-side.
    --
    -- NOTE: The timer may be rather wobbly unless you compile
    -- with the @-threaded@ option.

    -- * Documentation
    Timer, timer,
    interval, running, tick, start, stop,
    ) where

import Data.Typeable
import Control.Monad (when, forever, void)
import Control.Concurrent
import Control.Concurrent.STM
import Reactive.Threepenny

import Graphics.UI.Threepenny.Core


data Timer = Timer
    { tRunning  :: GetSet Bool Bool
    , tInterval :: GetSet Int Int   -- in ms
    , tTick     :: Event ()
    } deriving (Typeable)

-- | Create a new timer
timer :: Int -> Dynamic  Timer
timer time = do
    tvRunning     <- liftIO$ newTVarIO False
    tvInterval    <- liftIO$ newTVarIO time
    (tTick, fire) <- newEvent

    liftIO$ forkIO $ forever $ do
        atomically $ do
            b <- readTVar tvRunning
            when (not b) retry
        wait <- atomically $ readTVar tvInterval
        fire ()
        threadDelay (wait * 1000)

    let tRunning  = fromTVar tvRunning
        tInterval = fromTVar tvInterval

    return $ Timer {..}

-- | Timer event.
tick :: Timer -> Event ()
tick = tTick

-- | Timer interval in milliseconds.
interval :: Attr Timer Int
interval = fromGetSet tInterval

-- | Whether the timer is running or not.
running :: MonadIO m =>  ReadWriteAttrMIO m Timer Bool Bool
running = fromGetSet tRunning

-- | Start the timer.
start :: MonadIO m =>Timer -> m ()
start = set' running True

-- | Stop the timer.
stop :: MonadIO m =>Timer -> m ()
stop = set' running False

fromTVar :: TVar a -> GetSet a a
fromTVar var = (atomically $ readTVar var, atomically . writeTVar var)

type GetSet i o = (IO o, i -> IO ())

fromGetSet :: MonadIO m => (x -> GetSet i o) -> ReadWriteAttrMIO m x i o
fromGetSet f = mkReadWriteAttr (liftIO . fst . f) (\i x -> liftIO $ snd (f x) i)


{-----------------------------------------------------------------------------
    Small test
------------------------------------------------------------------------------}
{-

testTimer = do
    t <- timer
    void $ register (tick t) $ const $ putStr "Hello"
    return t
        # set interval 1000
        # set running True
-}
