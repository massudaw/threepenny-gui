{-# LANGUAGE BangPatterns,RecordWildCards, RecursiveDo #-}
module Reactive.Threepenny.PulseLatch (
    Pulse, newPulse, addHandler,
    neverP, mapP, filterJustP, unionWithP, unsafeMapIOP,dependOn,

    Latch,
    pureL, mapL,applyL, accumL, applyP,
    readLatch,
    ) where


import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import Control.Monad.Trans.Class
import Debug.Trace
import Control.Monad.Trans.State as Monad

import Data.IORef
import Data.Monoid (Endo(..))

import           Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.Vault.Strict   as Vault
import           Data.Unique.Really

import Reactive.Threepenny.Monads
import Reactive.Threepenny.Types
import System.IO.Unsafe

type Map = Map.HashMap

{-----------------------------------------------------------------------------
    Pulse
------------------------------------------------------------------------------}
-- Turn evaluation action into pulse that caches the value.
cacheEval :: EvalP (Maybe a) -> Build (Pulse a)
cacheEval e = do
    key <-  Vault.newKey
    return $ Pulse
        { addHandlerP = \_ -> return (return ())
        , evalP       = do
            vault <- Monad.get
            case Vault.lookup key vault of
                Just a  -> return a
                Nothing -> do
                    a <- e
                    Monad.put $ Vault.insert key a vault
                    return a
        }

-- Add a dependency to a pulse, for the sake of keeping track of dependencies.
dependOn :: Pulse a -> Pulse b -> Pulse a
dependOn p q = p { addHandlerP = \h -> do
  i <- addHandlerP q h
  j <- addHandlerP p h
  return (i  >> j ) }

-- Execute an action when the pulse occurs
whenPulse :: Pulse a -> (a -> IO ()) -> Handler
whenPulse p f = do
    ma <- evalP p
    case ma of
        Just a  -> return (f a)
        Nothing -> return $ return ()

first f (i,j) = (f i,j)

addHandlerRef :: IORef (Map Unique Handler,Map Unique Handler) -> ((Unique, Priority), Handler) -> Build (IO ())
addHandlerRef handlersRef ((uid,DoLatch),m) = do
          modifyIORef' handlersRef (first (Map.insert uid m))
          return $ modifyIORef' handlersRef (first (Map.delete  uid))
addHandlerRef handlersRef ((uid,DoIO),m) = do
          modifyIORef' handlersRef (fmap (Map.insert uid m))
          return $ modifyIORef' handlersRef (fmap (Map.delete uid ))

{-----------------------------------------------------------------------------
    Interface to the outside world.
------------------------------------------------------------------------------}
-- | Create a new pulse and a function to trigger it.
newPulse :: Build (Pulse a, a -> IO (),IO())
newPulse = do
    key         <- Vault.newKey
    handlersRef <- newIORef (Map.empty,Map.empty)      -- map of handlers

    let
        -- add handler to map
        addHandlerP  = addHandlerRef handlersRef

        -- evaluate all handlers attached to this input pulse
        fireP a = do
            let pulses = Vault.insert key (Just a) $ Vault.empty
            (handlersLatch,handlersIO) <- readIORef handlersRef
            ((msi,msj), _)  <- runEvalP pulses $ do
                i <- sequence handlersLatch
                j <- sequence handlersIO
                return ( i ,j)
            sequence_ msi
            sequence_ msj

        evalP = join . Vault.lookup key <$> Monad.get


        cleanP = do
          writeIORef handlersRef (Map.empty , Map.empty)
          return ()
    return (Pulse {..}, fireP,cleanP)

-- | Register a handler to be executed whenever a pulse occurs.
addHandler :: Pulse a -> (a -> IO ()) -> Build (IO ())
addHandler p f = do
    uid <- newUnique
    addHandlerP p ((uid, DoIO), whenPulse p f)



{-----------------------------------------------------------------------------
    Pulse and Latch
    Public API
------------------------------------------------------------------------------}
-- | Create a new pulse that never occurs.
neverP :: Pulse a
neverP = Pulse
    { addHandlerP = const $ return (return ())
    , evalP       = return Nothing
    }

-- | Map a function over pulses.
mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p = (`dependOn` p) <$> cacheEval (return . fmap f =<< evalP p)


-- | Map an IO function over pulses. Is only executed once.
unsafeMapIOP :: (a -> IO b) -> Pulse a -> Build (Pulse b)
unsafeMapIOP f p = (`dependOn` p) <$> cacheEval (traverse . fmap f =<< evalP p)
    where
    traverse :: Maybe (IO a) -> EvalP (Maybe a)
    traverse Nothing  = return Nothing
    traverse (Just m) = Just <$> lift m

-- | Filter occurrences. Only keep those of the form 'Just'.
filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p = (`dependOn` p) <$> cacheEval (return . join =<< evalP p)

-- | Pulse that occurs when either of the pulses occur.
-- Combines values with the indicated function when both occur.
unionWithP :: (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWithP f p q = (`dependOn` q) . (`dependOn` p) <$> cacheEval eval
    where
    eval = do
        x <- evalP p
        y <- evalP q
        return $ case (x,y) of
            (Nothing, Nothing) -> Nothing
            (Just a , Nothing) -> Just a
            (Nothing, Just a ) -> Just a
            (Just a1, Just a2) -> Just $ f a1 a2

-- | Apply the current latch value whenever the pulse occurs.
applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP l p = (`dependOn` p) <$> cacheEval eval
    where
    eval = do
        f <- lift $ readL l
        a <- evalP p
        return $ f <$> a

-- | Accumulate values in a latch.
accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a,IO ())
accumL a p1 = do
    -- IORef to hold the current latch value
    latch <- newIORef a
    let l1 = Latch { readL = readIORef latch }

    -- calculate new pulse from old value
    let l2 = mapL (flip ($)) l1
    p2 <- applyP l2 p1

    -- register handler to update latch
    uid <- newUnique
    let handler = whenPulse p2 $ (writeIORef latch $!)
    unH <- addHandlerP p2 ((uid, DoLatch), handler)

    return (l1,p2,unH)

-- | Latch whose value stays constant.
pureL :: a -> Latch a
pureL a = Latch { readL = return a }

-- | Map a function over latches.
--
-- Evaluated only when needed, result is not cached.
mapL :: (a -> b) -> Latch a -> (Latch b)
mapL f l =
  Latch { readL = f <$> readL l }

{-# RULES
"mapL/mapL" forall f g xs . mapL f (mapL g xs) = mapL (f . g) xs
 #-}
{-# RULES

"identity" forall f x .  mapL f  (pureL x) = pureL (f x);
"applicative homomorphism" forall f x .  pureL f `applyL` pureL x = pureL (f x);
"applicative interchange" forall u y .  u `applyL` pureL y = pureL ($ y) `applyL` u
 #-}



-- | Read the value of a 'Latch' at a particular moment in Build.
readLatch :: Latch a -> Build a
readLatch l = readL l

-- | Apply two current latch values
--
-- Evaluated only when needed, result is not cached.
applyL :: Latch (a -> b) -> Latch a -> Latch b
applyL l1 l2 = Latch { readL = readL l1 <*> readL l2 }

{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
test :: IO (Int -> IO ())
test = do
    (p1, fire,_) <- newPulse
    p2     <- mapP ((+2).) =<< mapP (+) p1
    (l1,_,_) <- accumL 0 p2
    let l2 =  mapL const l1
    p3     <- applyP l2 p1
    void $ addHandler p3 print
    return fire

test_recursion1 :: IO (IO ())
test_recursion1 = mdo
    (p1, fire,_) <- newPulse
    p2      <- applyP l2 p1
    p3      <- mapP (const (+1)) p2
    ~(l1,_,_) <- accumL (0::Int) p3
    let l2  =  mapL const l1
    void $ addHandler p2 print
    return $ fire ()
