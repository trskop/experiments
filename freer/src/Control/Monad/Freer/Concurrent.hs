{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Control.Monad.Freer.Concurrent
    -- TODO: Is the module name still valid?
  where

import Prelude (undefined)

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Exception (SomeException)
import Data.Either (Either)
import Data.Function (($), (.))
import Data.Maybe (Maybe)
import Data.Proxy (Proxy)
import System.IO (IO)
import qualified System.IO as IO (print)

--import Control.Monad.Base (MonadBase)
import Data.ByteString (ByteString)
import Haxl.Core.Monad (Env, GenHaxl)
import qualified Haxl.Core.Monad as Haxl (runHaxl)

import Control.Monad.Freer.Class (EffMonadBase, liftBaseEff)
import Control.Monad.Freer.IO (io)
import Control.Monad.Freer (Eff, Member)
import qualified Control.Monad.Freer as Eff (send)
import qualified Control.Monad.Freer.Internal as Eff.Internal (handleRelay)


type Handle = Proxy

data Accept c r where
    Accept :: Accept c c
    TryAccept :: Accept c (Maybe c)

data Connection c r where
    Receive :: Connection c ByteString
    Send :: ByteString -> Connection c ()

receive
    :: forall handle effs c
    . Member (Connection c) effs
    => handle c
    -> Eff effs ByteString
receive _ = Eff.send (Receive :: Connection c ByteString)

send
    :: forall handle effs c
    . Member (Connection c) effs
    => handle c
    -> ByteString
    -> Eff effs ()
send _ b = Eff.send (Send b :: Connection c ())

runConnection
    :: forall effs t r m
    . (EffMonadBase IO m effs)
    => IO ByteString
    -- ^ Read a 'ByteString' from a connection.
    -> (ByteString -> IO ())
    -- ^ Write a 'ByteString' in to a connection.
    -> Handle t
    -> Eff (Connection t ': effs) r
    -> Eff effs r
runConnection connRead connWrite _ = Eff.Internal.handleRelay pure $ \case
    Receive    -> (liftBaseEff connRead >>=)
    Send bytes -> (liftBaseEff (connWrite bytes) >>=)

withReadWrite
    :: (EffMonadBase IO m effs{-, HasReadWrite c-})
    => (ByteString -> Either SomeException a)
    ->  ( forall t. Handle t
        -> Eff (Connection t ': effs) ()
        -> Eff (ReadWrite t a ': effs) ()
        )
withReadWrite = undefined

-- | Type parameters @t@ and @a@ have following meaning:
--
-- * @t@ is used to distinguish various connections\/chanels\/etc. from
--   each other,
--
-- * and @a@ is a message type that can be written in to a
--   connection\/chanel\/etc.
data ReadWrite t a r where
    Read :: {- Proxy# t -> -} ReadWrite t a a
    Write :: {- Proxy# t -> -} a -> ReadWrite t a ()

runReadWrite
    :: (Handle t -> Eff effs a)
    -- ^ Read a message from a connection\/chanel\/etc.
    -> (Handle t -> a -> Eff effs ())
    -- ^ Write a message in to a connection\/chanel\/etc.
    -> Handle t
    -> Eff (ReadWrite t a ': effs) r
    -> Eff effs r
runReadWrite readOp writeOp p = Eff.Internal.handleRelay pure $ \case
    Read      -> (readOp p >>=)
    Write msg -> (writeOp p msg >>=)

read
    :: forall handle effs t a
    . Member (ReadWrite t a) effs
    => handle t
    -> Eff effs a
read _ = Eff.send (Read :: ReadWrite t a a)

write
    :: forall handle effs t a
    . Member (ReadWrite t a) effs
    => handle t
    -> a
    -> Eff effs ()
write _ msg = Eff.send (Write msg :: ReadWrite t a ())

data Haxl u r where
    Haxl :: GenHaxl u r -> Haxl u r

haxlDo :: Member (Haxl u) effs => GenHaxl u r -> Eff effs r
haxlDo = Eff.send . Haxl

runHaxl
    :: EffMonadBase IO m effs
    => Env u
    -> Eff (Haxl u ': effs) a
    -> Eff effs a
runHaxl env = Eff.Internal.handleRelay pure $ \case
    Haxl genHaxl -> (liftBaseEff (Haxl.runHaxl env genHaxl) >>=)

example :: Handle t -> Eff '[ReadWrite t ByteString, IO] ()
example h = read h >>= io . (IO.print :: ByteString -> IO ())
