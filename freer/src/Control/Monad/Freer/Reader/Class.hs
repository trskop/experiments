{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Freer.Reader.Class
  where

import Data.Proxy (Proxy)
import Control.Monad.Freer
import Control.Monad.Freer.Reader (Reader)
import qualified Control.Monad.Freer.Reader as Freer


-- | Interface for reader-like effects.
class EffReader f e | f -> e where
    {-# MINIMAL (ask | reader), local #-}

    -- | Retrieves the current environment.
    ask :: Member f r => Proxy f -> Eff r e
    ask proxy = reader proxy id

    -- | Executes a computation in a modified environment.
    local
        :: Member f r
        => Proxy f
        -> (e -> e)
        -- ^ Function that modifies the environment.
        -> Eff r a
        -- ^ Action executed with modified environment.
        -> Eff r a

    -- | Retrieves value based on the current environment.
    reader
        :: Member f r
        => Proxy f
        -> (e -> a)
        -- ^ Function applied to the environment before returning a value.
        -> Eff r a
    reader proxy f = f <$> ask proxy

instance EffReader (Reader e) e where
    ask _proxy = Freer.ask
    local _proxy = Freer.local

-- | Alias for 'reader'.
asks
    :: (Member f r, EffReader f e)
    => Proxy f
    -> (e -> a)
    -- ^ Function applied to the environment before returning a value.
    -> Eff r a
asks = reader
