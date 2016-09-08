{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-} -- runJoin
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Freer.Class
  where

import Control.Applicative (Alternative(..), Applicative(pure))
import Control.Monad (Monad, join)
import Data.Bool (Bool(False))
import Data.Function ((.), id)
import Data.Functor ((<$>))
import Data.Type.Equality (type (==))

import Control.Monad.Base (MonadBase(liftBase))

import Control.Monad.Freer (Eff, Member, NonDetEff, makeChoiceA, send)
import qualified Control.Monad.Freer.Internal as Internal
    ( Eff(E, Val)
    , run
    , runM
    , qComp
    )
import Data.FTCQueue (tsingleton)
import Data.Open.Union (weaken)


-- | Lift 'Eff' monad in to a monad that natively supports specified effects
-- (@effs@).
class Monad m => MonadEff effs m where
    liftEff :: Eff effs a -> m a

-- | Variant of 'liftEff' which allows effects to be specified explicitly using
-- a proxy type. This is useful in cases when type inference would fail without
-- explicit proof that sets of effects are equal.
liftEffP :: MonadEff effs m => proxy effs -> Eff effs a -> m a
liftEffP _proxy = liftEff

-- | 'Eff' monad can be embedded in to itself.
instance MonadEff effs (Eff effs) where
    liftEff = id

-- | 'Eff' monad with less effects can be injected in to an 'Eff' with strictly
-- more effects.
--
-- @
-- 'liftEff' = 'weakenEff'
-- @
instance MonadEff effs (Eff (eff ': effs)) where
    liftEff = weakenEff

weakenEff :: Eff effs a -> Eff (eff ': effs) a
weakenEff = \case
    Internal.Val x -> Internal.Val x
    Internal.E u q -> Internal.E (weaken u) (tsingleton k)
      where
        k = q `Internal.qComp` weakenEff

-- | @'Eff' '[]@ is isomorphic to 'Identity', therefore it can be lifted in to
-- any monad.
--
-- @
-- runIdentity . 'liftEff' === 'Internal.run'
-- @
instance Monad m => MonadEff '[] m where
    liftEff = pure . Internal.run

-- | @'Eff' '[m]@, where @m@ is a 'Monad', is isomorphic to just @m@.
instance Monad m => MonadEff '[m] m where
    liftEff = Internal.runM

liftEffJoin :: MonadEff effs m => Eff effs (m a) -> m a
liftEffJoin = join . liftEff

runJoin
    :: Monad m
    => (forall a. Eff (eff ': effs) a -> Eff effs (m a))
    -> Eff (eff ': effs) (m r)
    -> Eff effs (m r)
runJoin runE e = join <$> runE e

-- | 'NonDetEff' is interpreted in terms of underlying monad. Useful in example
-- when underlying monad is a monadic parser.
--
-- >>> :{
-- >>> liftEffP (Proxy :: Proxy '[NonDetEff, Maybe]) empty
-- >>>     :: Maybe Int
-- >>> :}
-- Nothing
--
-- >>> :{
-- >>> liftEffP (Proxy :: Proxy '[NonDetEff, Maybe]) (empty <|> pure 1)
-- >>>     :: Maybe Int
-- >>> :}
-- Just 1
instance (Alternative m, Monad m) => MonadEff '[NonDetEff, m] m where
    liftEff = liftEffJoin . makeChoiceA

-- {{{ LastMember -------------------------------------------------------------

class
    ( Member eff effs
    ) => LastMember (eff :: * -> *) (effs :: [* -> *])
    | effs -> eff


-- | Function 'send' restricted to sending a last effect.
sendLast :: LastMember eff effs => eff a -> Eff effs a
sendLast = send

instance LastMember m '[m]
instance
    ( (any1 == m) ~ 'False
    , (any2 == m) ~ 'False
    , Member m (any1 ': any2 ': effs)
    , LastMember m effs
    ) => LastMember m (any1 ': any2 ': effs)

-- }}} LastMember -------------------------------------------------------------

-- {{{ MonadBase --------------------------------------------------------------

instance (LastMember m effs, MonadBase b m) => MonadBase b (Eff effs) where
    liftBase = sendLast . (liftBase :: b a -> m a)

type EffMonadBase b m effs = (LastMember m effs, MonadBase b (Eff effs))

-- | Function 'liftBase' with 'Eff' friendly type signature.
liftBaseEff :: EffMonadBase b m effs => b a -> Eff effs a
liftBaseEff = liftBase

-- }}} MonadBase --------------------------------------------------------------
