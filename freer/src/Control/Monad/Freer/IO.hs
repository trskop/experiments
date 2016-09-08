{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Freer.IO
  where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as MonadIO (MonadIO(liftIO))
import Data.Function ((.))
import System.IO (IO)

import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.Freer.Class (EffMonadBase)


-- | Lift 'IO' in to 'Eff' when 'IO' is its base monad. It's just a specialized
-- 'send':
--
-- @
-- 'io' = 'send'
-- @
--
-- Using 'io' instead of 'send' has several advantages:
--
-- * Better type errors, in some cases,
--
-- * and more readable code.
io :: Member IO effs => IO a -> Eff effs a
io = send

-- | Shorthand for @'io' . 'void'@
voidIO :: Member IO effs => IO a -> Eff effs ()
voidIO = io . void

-- | Version of 'MonadIO.liftIO' for 'Eff' monad.
--
-- @
-- 'io' === 'liftIO' (Proxy :: Proxy IO)
-- @
liftIO
    :: forall proxy m effs a. (MonadIO m, Member m effs)
    => proxy m
    -> IO a
    -> Eff effs a
liftIO _ = send . (MonadIO.liftIO :: IO a -> m a)
