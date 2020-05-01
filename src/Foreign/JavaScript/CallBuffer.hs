{-# LANGUAGE TupleSections,RecordWildCards #-}
module Foreign.JavaScript.CallBuffer  where

import           Control.Concurrent.STM       as STM
import           Control.Monad                           (unless)
import qualified Data.Aeson                   as JSON
import qualified Control.Exception as E
import           Foreign.JavaScript.Types
import           Foreign.RemotePtr            as Foreign
import Data.List (any,intercalate)
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.IntSet as Set
import qualified Data.Foldable as F
import Control.Monad
import Data.Time
import Debug.Trace
import GHC.Conc
import GHC.Clock


{-----------------------------------------------------------------------------
    Call Buffer
------------------------------------------------------------------------------}
flushCallBuffer :: Window -> IO ()
flushCallBuffer w = do 
  atomically . flushCallBufferSTM $ w

-- | Set the call buffering mode for the given browser window.
setCallBufferMode :: Window -> CallBufferMode -> IO ()
setCallBufferMode w@Window{..} new = atomically $ do
    writeTVar wCallBufferMode new

-- | Flush the call buffer,
-- i.e. send all outstanding JavaScript to the client in one single message.
flushCallEvalBuffer :: Window -> TMVar Result -> STM ()
flushCallEvalBuffer w@Window{..} ref = do
        code <- readTVar wCallBuffer
        writeTVar wCallBuffer  id
        callEval ref code
        return ()

flushCallBufferSTM :: Window -> STM ()
flushCallBufferSTM w@Window{..} = do
        code <- readTVar wCallBuffer
        writeTVar wCallBuffer id
        runEval  code
        return ()

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferCallEval :: Window -> TMVar Result -> String -> STM ()
bufferCallEval w m c = bufferCallEval' w m (snockBuffer c)

bufferCallEval' :: Window -> TMVar Result -> CallBuffer -> STM ()
bufferCallEval' w@Window{..} ref icode =  do
  action <- do
        mode <- readTVar wCallBufferMode
        let buffer = do
                modifyTVar wCallBuffer (appendBuffer icode)
                return Nothing

        case mode of
            BufferAll -> buffer
            BufferCall -> buffer
            i -> do
              return $ Just icode
  case action of
    Nothing -> flushCallEvalBuffer w ref
    Just i -> callEval ref i

forceObject :: Window -> JSObject -> IO ()
forceObject  w@Window{..} top = do
  root <- unprotectedGetCoupon top
  void $ atomically $ do
    action <- do
         (rendered,map) <- readTVar wCallBufferMap
         if Set.member root rendered
            then  return Nothing
            else
              case findBM root map of
                Just (s,m) ->  do
                  writeTVar wCallBufferMap (Set.union s rendered,deleteBM root map)
                  msg <- readTVar m
                  return $Just msg
                Nothing ->   do
                  writeTVar wCallBufferMap (Set.insert root rendered,map)
                  return Nothing
    traverse (bufferRunEval' w ) action

flushChildren :: Window -> JSObject -> JSObject -> IO ()
flushChildren w@Window{..} top child = do
  root <- unprotectedGetCoupon top
  childs <- unprotectedGetCoupon child
  void $ atomically $ do
    mode <- readTVar wCallBufferMode
    let childFun c = do
            (rendered, map) <- readTVar wCallBufferMap
            if Set.member c rendered
              then return Nothing
              else
                case findBM c map of
                  Just (k,ref) ->  fmap (Just .(k,))$ readTVar ref
                  Nothing -> return Nothing
    action <- do
        (rendered,map) <- readTVar wCallBufferMap
        childActions <- childFun childs
        if Set.member root rendered
           then
             case childActions of
               Just (k,v) ->  do
                 writeTVar wCallBufferMap (Set.insert root (Set.union k rendered), deleteBM childs map)
                 return (Just v)
               i ->  do
                 writeTVar wCallBufferMap (Set.insert root  $ Set.insert childs $ rendered, map)
                 return Nothing
           else do
             let val = findBM root map
             case childActions of
               Just (kc,act) -> do
                 case val of
                   Just (k,v) ->  do
                     modifyTVar v  (flip appendBuffer act)
                     out <- readTVar v
                     writeTVar wCallBufferMap (rendered , moveAtBM k childs map)
                     return Nothing
                   Nothing ->  do
                     writeTVar wCallBufferMap (rendered , (\(k,v) -> if Set.member childs k then (Set.insert root k,v) else (k,v)) <$> map)
                     return Nothing
               Nothing -> do
                 case val of
                   Just (kr,_) -> do
                     writeTVar wCallBufferMap (rendered , (\(k,v) -> if Set.member root k then (Set.insert childs k,v) else (k,v)) <$>  map)
                     return Nothing
                   Nothing ->  do
                     var <- newTVar id
                     writeTVar wCallBufferMap (rendered , insertSBM (Set.fromList [root,childs]) var map)
                     return Nothing
    traverse (bufferRunEval' w ) action

appendBuffer xs ys = ys . xs
snockBuffer e =  (e:)

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEvalMethod :: Window -> JSObject -> String -> IO ()
bufferRunEvalMethod w js s = do
  coupon <- unprotectedGetCoupon js
  atomically $ bufferRunEvalMethod' w coupon (snockBuffer s)

bufferRunEvalMethod' :: Window -> Coupon -> CallBuffer -> STM ()
bufferRunEvalMethod'  w@Window{..} coupon  icode = void $ do
    mode <- readTVar wCallBufferMode
    action <- do
       (rendered,map )<- readTVar wCallBufferMap
       if Set.member coupon rendered
         then return (Just icode)
         else  do
            case findBM coupon map of
              Just (k,ref) -> do
                modifyTVar ref (appendBuffer icode)
              Nothing -> do
                ref <- newTVar icode
                writeTVar wCallBufferMap (rendered,insertBM coupon ref map)
            return Nothing
    traverse (bufferRunEval' w ) action

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEval w icode = bufferRunEval' w (snockBuffer icode)
bufferRunEval' :: Window -> CallBuffer -> STM ()
bufferRunEval' w@Window{..} icode = do
      mode <- readTVar wCallBufferMode
      let buffer = do
              modifyTVar wCallBuffer (appendBuffer icode)
              t0 <- unsafeIOToSTM getMonotonicTimeNSec 
              modifyTVar wCallBufferStats (\(ti,tf,i) -> (ti,t0,i+1))
              return Nothing
      o <- case mode of
            BufferAll ->  buffer
            BufferRun -> buffer
            i-> return $ Just icode
      traverse runEval o
      return ()

newJSPtr w coupon h1 h2 = do
  o <- newRemotePtr coupon  h1 h2
  addFinalizer o . atomically $ do
    bufferRunEvalMethod' w coupon (snockBuffer $ "Haskell.freeStablePtr('" ++ show coupon++ "')")
    modifyTVar (wCallBufferMap w) (\(k,v) -> (Set.delete coupon k ,v))
  return o
