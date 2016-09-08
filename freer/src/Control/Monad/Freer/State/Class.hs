{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Freer.State.Class
  where

import Data.Proxy (Proxy)
import Control.Monad.Freer
import Control.Monad.Freer.State (State)
import qualified Control.Monad.Freer.State as Freer


-- | Interface for state-like effects.
class EffState s v | s -> v where
    {-# MINIMAL state | get, put #-}

    get :: Member s effs => Proxy s -> Eff effs v
    get proxy = state proxy $ \s -> (s, s)

    put :: Member s effs => Proxy s -> v -> Eff effs ()
    put proxy s = state proxy $ const ((), s)

    state :: Member s effs => Proxy s -> (v -> (a, v)) -> Eff effs a
    state proxy f = do
        (a, s) <- f <$> get proxy
        a <$ put proxy s

instance EffState (State v) v where
    get _proxy = Freer.get
    put _proxy = Freer.put

gets :: (Member s effs, EffState s v) => Proxy s -> (v -> a) -> Eff effs a
gets proxy f = f <$> get proxy

-- | Modify state value using provided function:
--
-- @
-- 'modify' (Proxy @(State Int)) ((+1) :: Int -> Int)
--    :: ('Member' (State Int) effs, 'EffState' (State Int) Int)
--    => Proxy s
--    -> 'Eff' effs ()
-- @
modify :: (Member s effs, EffState s v) => Proxy s -> (v -> v) -> Eff effs ()
modify proxy f = state proxy $ \s -> ((), f s)

-- | A variant of 'modify' in which the computation is strict in the new state.
modify' :: (Member s effs, EffState s v) => Proxy s -> (v -> v) -> Eff effs ()
modify' proxy f = state proxy $ \s -> let s' = f s in s' `seq` ((), s')
