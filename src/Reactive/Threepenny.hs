{-# LANGUAGE TupleSections,BangPatterns,RecursiveDo  #-}
module Reactive.Threepenny (
    -- * Synopsis
    -- | Functional reactive programming.

    -- * Types
    -- $intro
    Event, Behavior,Dynamic,
    runDynamic,execDynamic,registerDynamic,closeDynamic,

    -- * IO
    -- | Functions to connect events to the outside world.
    Handler, newEvent, register,onEventIO,
    currentValue,

    -- * Core Combinators
    -- | Minimal set of combinators for programming with 'Event' and 'Behavior'.
    module Control.Applicative,
    never, filterJust, unionWith,
    accumE, apply, stepper, stepperT,
    emap,
    -- $classes

    -- * Derived Combinators
    -- | Additional combinators that make programming
    -- with 'Event' and 'Behavior' convenient.
    -- ** Application
    (<@>), (<@),
    -- ** Filtering
    filterE,emap,bmap,tmap, filterApply, whenE, split,
    -- ** Union
    unions, concatenate,
    -- ** Accumulation
    -- $accumulation
    accumB, mapAccum,

    -- * Additional Notes
    -- $recursion

    -- * Tidings
    Tidings, tidings, facts, rumors,
    accumT,

    -- * Internal
    -- | Functions reserved for special circumstances.
    -- Do not use unless you know what you're doing.
    onChange,onChangeDyn,onChangeDynIni, unsafeMapIO, newEventsNamed,mapEventIO,mapEventDyn,mapEventDynInterrupt,mapTidingsDyn0,mapTidingsDyn,mapTidingsDynInterrupt0,mapTidingsDynInterrupt,onEventDyn,onEventDynIni,onEventDynInterrupt
    ) where

import Debug.Trace
import Control.Applicative
import Control.Concurrent
import Control.Monad (void,(>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import qualified Data.Map as Map

import           Reactive.Threepenny.Memo       as Memo
import qualified Reactive.Threepenny.PulseLatch as Prim
import qualified Control.Monad.Trans.State.Strict as State
import System.IO.Unsafe

type Pulse = Prim.Pulse
type Latch = Prim.Latch
type Map   = Map.Map
type Dynamic  = State.StateT [IO ()] IO


runDynamic :: Dynamic a -> IO (a,([IO()]))
runDynamic w = do
  (v,f) <- State.runStateT w []
  return (v,f)


execDynamic :: Dynamic a -> IO [IO()]
execDynamic w = do
  State.execStateT w ([])


registerDynamic :: IO () -> Dynamic  ()
registerDynamic w = State.modify' (\(i) -> (w:i))

closeDynamic :: Dynamic a -> IO a
closeDynamic  m = do
  (i,v) <- runDynamic m
  sequence_ v
  return i


{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
{- $intro

At its core, Functional Reactive Programming (FRP) is about two
data types 'Event' and 'Behavior' and the various ways to combine them.

-}

{-| @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event a = [(Time,a)]
-}
newtype Event    a = E { unE :: Memo (Pulse a) }

{-| @Behavior a@ represents a value that varies in time. Think of it as

> type Behavior a = Time -> a
-}
data  Behavior a = B { latch :: Latch a, changes :: Event () }

{------------------u----------------------------------------------------------
    IO
------------------------------------------------------------------------------}
-- | An /event handler/ is a function that takes an
-- /event value/ and performs some computation.
type Handler a = a -> IO ()

-- | Create a new event.
-- Also returns a function that triggers an event occurrence.
newEvent :: Dynamic (Event a, Handler a)
newEvent = do
    (p, fire,fin) <- liftIO$ Prim.newPulse
    registerDynamic fin
    return (E $ fromPure p, fire)


-- | Create a series of events with delayed initialization.
--
-- For each name, the initialization handler will be called
-- exactly once when the event is first "brought to life",
-- e.g. when an event handler is registered to it.
newEventsNamed :: Ord name
   => Handler (name, Event a, Handler a )   -- ^ Initialization procedure.
    -> IO (name -> IO(Event a))                 -- ^ Series of events.
newEventsNamed init = do
    eventsRef <- newIORef Map.empty
    return $ \name  -> return $ E $ memoize $ do
        events <- readIORef eventsRef
        case Map.lookup name events of
            Just p  -> return p
            Nothing -> do
                (p, fire,fin) <- Prim.newPulse
                writeIORef eventsRef $ Map.insert name p events
                init (name, E $ fromPure p, fire)
                return p


-- | Register an event 'Handler' for an 'Event'.
-- All registered handlers will be called whenever the event occurs.
--
-- When registering an event handler, you will also be given an action
-- that unregisters this handler again.
--
-- > do unregisterMyHandler <- register event myHandler
--
register :: Event a -> Handler a -> Dynamic ()
register e h = do
    p <- liftIO$ at (unE e)     -- evaluate the memoized action
    r <- liftIO $ Prim.addHandler p h
    registerDynamic r
    return ()


-- | Register an event 'Handler' for a 'Behavior'.
-- All registered handlers will be called whenever the behavior changes.
--
-- However, note that this is only an approximation,
-- as behaviors may change continuously.
-- Consequently, handlers should be idempotent.
onChange :: Behavior a -> Handler a -> Dynamic ()
onChange (B l e) h = do
    -- This works because latches are updated before the handlers are being called.
    register e (\_ -> h =<< Prim.readLatch l)

onChangeDynIni :: [IO ()] -> Behavior a -> (a -> Dynamic ()) -> Dynamic ()
onChangeDynIni ini (B l e ) hf = mdo
    -- This works because latches are updated before the handlers are being called.
    (ev,h) <-newEvent
    register ((,) <$> bv <@> e) (\(~(fin,i))-> sequence_ fin >> Prim.readLatch l >>= (execDynamic . hf   >=> h))
    bv <- stepper ini  ev
    registerDynamic ( currentValue bv >>= sequence_)
    return ()



onChangeDyn :: Behavior a -> (a -> Dynamic ()) -> Dynamic ()
onChangeDyn b@(B  l e ) hf = mdo
    inil <- liftIO (Prim.readLatch l)
    v <- liftIO $ unsafeInterleaveIO $ execDynamic $ hf  inil
    onChangeDynIni v b hf
    {-
    (ev,h) <-newEvent
    register ((,) <$> bv <@> e) (\(~(fin,i))-> sequence_ fin >> Prim.readLatch l >>= (execDynamic . hf   >=> h))
    bv <- stepper v ev
    registerDynamic (currentValue bv >>= sequence_)
    return ()
    -}


-- | Read the current value of a 'Behavior'.
currentValue :: MonadIO m => Behavior a -> m a
currentValue (B l _) = liftIO $ Prim.readLatch l


{-----------------------------------------------------------------------------
    Core Combinators
------------------------------------------------------------------------------}
instance Functor Event where
  fmap  = emap
  {-# INLINE fmap #-}

emap :: (a ->b ) -> Event a -> Event b
emap f (E e) = E $ liftMemo1 (Prim.mapP f) e


{-# NOINLINE[1] emap #-}
{-# RULES
"emap/emap" forall f g xs . emap f (emap g xs) = emap (f . g) xs
 #-}

unsafeMapIO :: (a -> IO b) -> Event a -> Event b
unsafeMapIO f (E e) = E $ liftMemo1 (Prim.unsafeMapIOP f) e


-- | Event that never occurs.
-- Think of it as @never = []@.
never :: Event a
never = E $ fromPure Prim.neverP

-- | Return all event occurrences that are 'Just' values, discard the rest.
-- Think of it as
--
-- > filterJust es = [(time,a) | (time,Just a) <- es]
filterJust e = E $ liftMemo1 Prim.filterJustP (unE e)

-- | Merge two event streams of the same type.
-- In case of simultaneous occurrences, the event values are combined
-- with the binary function.
-- Think of it as
--
-- > unionWith f ((timex,x):xs) ((timey,y):ys)
-- >    | timex == timey = (timex,f x y) : unionWith f xs ys
-- >    | timex <  timey = (timex,x)     : unionWith f xs ((timey,y):ys)
-- >    | timex >  timey = (timey,y)     : unionWith f ((timex,x):xs) ys
unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f e1 e2 = E $ liftMemo2 (Prim.unionWithP f) (unE e1) (unE e2)

-- | Apply a time-varying function to a stream of events.
-- Think of it as
--
-- > apply bf ex = [(time, bf time x) | (time, x) <- ex]
apply :: Behavior (a -> b) -> Event a -> Event b
apply  f x        = E $ liftMemo1 (\p -> Prim.applyP (latch f) p) (unE x)

infixl 4 <@>, <@

-- | Infix synonym for 'apply', similar to '<*>'.
(<@>) :: Behavior (a -> b) -> Event a -> Event b
(<@>) = apply

-- | Variant of 'apply' similar to '<*'
(<@) :: Behavior a -> Event b -> Event a
b <@ e = (const <$> b) <@> e

-- | The 'accumB' function is similar to a /strict/ left fold, 'foldl''.
-- It starts with an initial value and combines it with incoming events.
-- For example, think
--
-- > accumB "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = stepper "x" [(time1,"xy"),(time2,"xyz")]
--
-- Note that the value of the behavior changes \"slightly after\"
-- the events occur. This allows for recursive definitions.

accumB :: a -> Event (a -> a) -> Dynamic  (Behavior a)
accumB a e = do
  (l1,p1,unH) <- liftIO$ Prim.accumL a =<< at (unE e)
  registerDynamic unH
  p2      <- liftIO$ Prim.mapP (const ()) p1
  return $ B l1 (E $ fromPure p2)

stepperT ::  a -> Event a -> Dynamic (Tidings a)
stepperT a e = accumT a (const <$> e)


-- | Construct a time-varying function from an initial value and
-- a stream of new values. Think of it as
--
-- > stepper x0 ex = return $ \time ->
-- >     last (x0 : [x | (timex,x) <- ex, timex < time])
--
-- Note that the smaller-than-sign in the comparision @timex < time@ means
-- that the value of the behavior changes \"slightly after\"
-- the event occurrences. This allows for recursive definitions.
stepper :: a -> Event a -> Dynamic (Behavior a)
stepper a e = accumB a (const <$> e)

-- | The 'accumE' function accumulates a stream of events.
-- Example:
--
-- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = return [(time1,"xy"),(time2,"xyz")]
--
-- Note that the output events are simultaneous with the input events,
-- there is no \"delay\" like in the case of 'accumB'.
accumE :: a -> Event (a -> a) -> Dynamic (Event a)
accumE a e = do
  (_,p,unH) <-  liftIO $ Prim.accumL a =<< at (unE e)
  registerDynamic unH
  return $ E $ fromPure p

accumT :: a -> Event (a -> a) -> Dynamic (Tidings a)
accumT a e = do
  (l1,p,unH) <-  liftIO $ Prim.accumL a =<< at (unE e)
  registerDynamic unH
  p2 <- liftIO$ Prim.mapP (const ()) p
  return $ tidings  (B l1 (E $ fromPure p2)) (E $ fromPure p)


instance Functor Behavior where
  fmap = bmap

bmap :: (a ->b ) -> Behavior a -> Behavior b
bmap f ~(B l e) = B (Prim.mapL f l) e
{-# NOINLINE [1] bmap #-}


instance Applicative Behavior where
  pure = pureB
  {-# INLINE pure #-}
  (<*>) = applyB
  {-# INLINE (<*>) #-}

pureB a  = B (Prim.pureL a) never
applyB ~(B lf ef) ~(B lx ex) =
        B (Prim.applyL lf lx) (unionWith const ef ex)

{-# NOINLINE[1] pureB  #-}
{-# NOINLINE[1] applyB #-}

{-# RULES

"identity" forall f x .  bmap f  (pureB x) = pureB (f x);
"applicative homomorphism" forall f x .  pureB f `applyB` pureB x = pureB (f x);
"applicative interchange" forall u y .  u `applyB` pureB y = pureB ($ y) `applyB` u
 #-}


{- $classes

/Further combinators that Haddock can't document properly./

> instance Applicative Behavior

'Behavior' is an applicative functor. In particular, we have the following functions.

> pure :: a -> Behavior a

The constant time-varying value. Think of it as @pure x = \\time -> x@.

> (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b

Combine behaviors in applicative style.
Think of it as @bf \<*\> bx = \\time -> bf time $ bx time@.

-}

{- $recursion

Recursion in the 'IO' monad is possible, but somewhat limited.
The main rule is that the sequence of IO actions must be known
in advance, only the values may be recursive.

Good:

> mdo
>     let e2 = apply (const <$> b) e1   -- applying a behavior is not an IO action
>     b <- accumB $ (+1) <$ e2

Bad:

> mdo
>     b <- accumB $ (+1) <$ e2          -- actions executed here could depend ...
>     let e2 = apply (const <$> b) e1   -- ... on this value

-}

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
-- | Return all event occurrences that fulfill the predicate, discard the rest.
filterE :: (a -> Bool) -> Event a -> Event a
filterE p = filterJust . fmap (\a -> if p a then Just a else Nothing)

-- | Return all event occurrences that fulfill the time-varying predicate,
-- discard the rest. Generalization of 'filterE'.
filterApply :: Behavior (a -> Bool) -> Event a -> Event a
filterApply bp = fmap snd . filterE fst . apply ((\p a -> (p a,a)) <$> bp)

-- | Return event occurrences only when the behavior is 'True'.
-- Variant of 'filterApply'.
whenE :: Behavior Bool -> Event a -> Event a
whenE bf = filterApply (const <$> bf)

-- | Split event occurrences according to a tag.
-- The 'Left' values go into the left component while the 'Right' values
-- go into the right component of the result.
split :: Event (Either a b) -> (Event a, Event b)
split e = (filterJust $ fromLeft <$> e, filterJust $ fromRight <$> e)
    where
    fromLeft  (Left  a) = Just a
    fromLeft  (Right b) = Nothing
    fromRight (Left  a) = Nothing
    fromRight (Right b) = Just b

-- | Collect simultaneous event occurrences in a list.
unions :: [Event a] -> Event [a]
unions = foldr (unionWith (++)) never . map (fmap (:[]))

-- | Apply a list of functions in succession.
-- Useful in conjunction with 'unions'.
--
-- > concatenate [f,g,h] = f . g . h
concatenate :: [a -> a] -> (a -> a)
concatenate = foldr (.) id

onEventIO :: Event a -> (a -> IO void) -> Dynamic ()
onEventIO e h =   register e (void . h)
{-# INLINE onEventIO #-}

onEventDyn
  :: Event a ->  (a -> Dynamic b) -> Dynamic ()
onEventDyn  e f =  mdo
  (efin,hfin) <- newEvent
  onEventIO ((,) <$> bfin <@>e ) (\ ~(fin,i) -> sequence_ fin >> (hfin  .snd =<< (runDynamic (f i) )) )
  bfin <- stepper [] efin
  registerDynamic $ ((sequence_ =<< currentValue bfin) )
  return ()

onEventDynIni
  :: [IO ()] -> Event a ->  (a -> Dynamic b) -> Dynamic ()
onEventDynIni  ini e f =  mdo
  (efin,hfin) <- newEvent
  onEventIO ((,) <$> bfin <@>e ) (\ ~(fin,i) -> sequence_ fin >> (hfin  .snd =<< (runDynamic (f i) )) )
  bfin <- stepper ini efin
  registerDynamic $ ((sequence_ =<< currentValue bfin) )
  return ()



onEventDynInterrupt
  :: Event a ->  (a -> Dynamic b) -> Dynamic ()
onEventDynInterrupt  e f =  mdo
  (efin,hfin) <- newEvent
  onEventIO ((,) <$> bfin <@>e )
    (\ ~(fin,i) -> do
      sequence_ fin
      forkIO (do
          pid <- myThreadId
          (i,s) <- runDynamic (f i)
          hfin ([sequence s >> killThread pid])))
  bfin <- stepper [] efin
  registerDynamic $ sequence_ =<< currentValue bfin
  return ()



{- $accumulation

Note: All accumulation functions are strict in the accumulated value!
acc -> (x,acc) is the order used by 'unfoldr' and 'State'.

-}

-- | Efficient combination of 'accumE' and 'accumB'.
mapAccum :: acc -> Event (acc -> (x,acc)) -> Dynamic (Event x, Behavior acc)
mapAccum acc ef = do
    e <- accumE (undefined,acc) ((. snd) <$> ef)
    b <- stepper acc (snd <$> e)
    return (fst <$> e, b)


{-----------------------------------------------------------------------------
    Tidings

    Data type for combining user events.
    See <http://apfelmus.nfshost.com/blog/2012/03/29-frp-three-principles-bidirectional-gui.html>
    for more information.
------------------------------------------------------------------------------}
-- | Data type representing a behavior ('facts')
-- and suggestions to change it ('rumors').
data Tidings a = T { facts :: Behavior a, rumors :: Event a }

-- | Smart constructor. Combine facts and rumors into 'Tidings'.
tidings :: Behavior a -> Event a -> Tidings a
tidings b e = T b e
{-# INLINE tidings #-}

instance Functor Tidings where
  fmap = tmap
  {-# INLINE fmap #-}

tmap :: (a ->b ) -> Tidings a -> Tidings b
tmap f (T b e) = T (bmap f b) (emap f e)

{-# NOINLINE [0] tmap #-}
{-# RULES
"tmap/tmap" forall f g xs . tmap f (tmap g xs) = tmap (f . g) xs;
 #-}

-- | The applicative instance combines 'rumors'
-- and uses 'facts' when some of the 'rumors' are not available.
instance Applicative Tidings where
    pure   = pureT
    {-# INLINE pure #-}
    (<*>)  = applyT
    {-# INLINE (<*>) #-}

pureT x =  T (pure x) never
applyT f x = uncurry ($) <$> pair f x

{-# NOINLINE[0] pureT  #-}
{-# NOINLINE[0] applyT #-}
{-# RULES
"identity" forall f x .  tmap f  (pureT x) = pureT (f x);
"applicative homomorphism" forall f x .  pureT f `applyT` pureT x = pureT (f x);
"applicative interchange" forall u y .  u `applyT` pureT y = pureT ($ y) `applyT` u
 #-}

pair :: Tidings a -> Tidings b -> Tidings (a,b)
pair (T bx ex) (T by ey) = T b e
    where
    b = (,) <$> bx <*> by
    x = flip (,) <$> by <@> ex
    y = (,) <$> bx <@> ey
    e = unionWith (\ ~(x,_) ~(_,y) -> (x,y)) x y

-- | Expose an event handler to cancel the forked computation
mapEventDynInterrupt ::(a -> Dynamic b) -> Event a -> Dynamic (Event b)
mapEventDynInterrupt f x = do
    (e,h) <- newEvent
    onEventDynInterrupt x (\i -> f i  >>= liftIO . h)
    return  e

mapEventIO :: (a -> IO b ) -> Event a -> Dynamic (Event b)
mapEventIO f x= do
    (e,h) <- newEvent
    onEventIO x (\i -> f i >>= h)
    return e

-- | Expose finalizers from the dynamic monad to clean up the event handlers.
mapEventDyn ::(a -> Dynamic b) -> Event a -> Dynamic (Event b)
mapEventDyn f x = do
    (e,h) <- newEvent
    onEventDyn x (\i -> f i  >>= liftIO . h)
    return  e

mapTidingsDyn0 :: a -> (a -> Dynamic b) -> Event a -> Dynamic (Tidings b)
mapTidingsDyn0 i f e = mdo
  ini <- liftIO $ runDynamic $ f i
  (efin,hfin) <- newEvent
  onEventIO ((,) <$> fmap snd (facts bfin) <@> e)
      (\ ~(fin,i) -> do
        sequence_ fin
        hfin =<< runDynamic (f i))
  rec bfin <- stepperT ini efin
  registerDynamic $ sequence_ =<< currentValue (snd <$> facts bfin)
  return (fst <$> bfin)

mapTidingsDyn :: (a -> Dynamic b) -> Tidings a -> Dynamic (Tidings b)
mapTidingsDyn  f x = do
  i <- currentValue  (facts x)
  mapTidingsDyn0 i f (rumors x)




mapTidingsDynInterrupt0 :: a -> (a -> Dynamic b) -> Event a -> Dynamic (Tidings b)
mapTidingsDynInterrupt0 i f e = mdo
  ini <- liftIO $ runDynamic $ f i
  (efin,hfin) <- newEvent
  onEventIO ((,) <$> fmap snd (facts bfin) <@> e)
      (\ ~(fin,i) -> do
        sequence_ fin
        forkIO (do
            pid <- myThreadId
            putStrLn $ "Start thread" ++ show pid
            (i,s) <- runDynamic (f i)
            hfin (i,[sequence s >> (putStrLn $ "Stop thread" ++ show pid) >> killThread pid ])))
  bfin <- stepperT ini efin
  registerDynamic $ sequence_ =<< currentValue (snd <$> facts bfin)
  return (fst <$> bfin)


mapTidingsDynInterrupt :: (a -> Dynamic b) -> Tidings a -> Dynamic (Tidings b)
mapTidingsDynInterrupt f x = do
  i <- currentValue  (facts x)
  mapTidingsDynInterrupt0 i f (rumors x)



{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
test :: Dynamic ()
test = do
  (e1,fire) <- newEvent
  e2 <- accumE 0 $ (+) <$> e1
  _  <- register e2 print

  liftIO $ fire 1
  liftIO $ fire 2
  return ()

test_recursion1 :: Dynamic ()
test_recursion1 = mdo
    (e1, fire) <- newEvent
    let e2 :: Event Int
        e2 = apply ((\i -> traceShow ("work",i ()) i)  <$> (const . snd <$> facts b)) e1
    b  <-  fmap (fmap ("t",)) <$> accumT 0 $ (+1) <$ e2
    let b2 = traceShowId . ("a",)<$> b

    onChange (facts b) (print . ("onChange",))
    onChange (facts b2) (print . ("onChange2",))
    register (e2 ) print
    liftIO $ fire ()
    liftIO $ fire ()
    liftIO $ fire ()

test_finalizer :: IO ()
test_finalizer = do
  (fire ,fin) <- runDynamic $ mdo
    (e1, fire) <- newEvent
    bi <- stepper 0 e1
    bj <- accumB 0 ((+) <$>e1)
    onChangeDyn (bi) (\i -> do
      liftIO $ print ("hold",i)
      registerDynamic ( print ("removeHold",i))
      onChangeDyn (bj) (\i -> do
        liftIO . print$ ("accum",i )
        registerDynamic ((print ("removeAccum",i)))))
    return fire
  liftIO $fire 1
  liftIO $ fire 2
  liftIO $ fire 3
  sequence_ fin
  return ()

test_finalizer_onevent :: Dynamic ()
test_finalizer_onevent = mdo
  (e1, fire) <- newEvent
  bi <- stepperT 0 e1
  bj <- accumT 0 ((+) <$>e1)
  onEventDyn (rumors bi) (\i -> do
    liftIO $ print ("hold",i)
    registerDynamic ( print ("removeHold",i))
    onEventDyn (rumors bj)  (\i -> do
      liftIO . print$ ("accum",i )
      registerDynamic (  (print ("removeAccum",i)))))
  liftIO $fire 1
  liftIO $ fire 2
  liftIO $ fire 3
  return ()




