module Reactive.Threepenny.Monads where

import Control.Monad.Trans.State.Lazy
import Reactive.Threepenny.Types

{-----------------------------------------------------------------------------
    EvalP - evaluate pulses
------------------------------------------------------------------------------}
runEvalP :: Values -> EvalP a -> IO (a, Values)
runEvalP pulses m = do
    (a, s) <- runStateT m pulses
    return (a, s)
{-# INLINE runEvalP #-}
