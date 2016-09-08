module Data.FTCQueue.Instances
  where

import Control.Category (Category)
import qualified Control.Category as Category

import Data.Semigroupoid (Semigroupoid(o))

import Data.FTCQueue (FTCQueue, (><), tsingleton)


instance Semigroupoid (FTCQueue f) where
    o = flip (><)

instance Applicative f => Category (FTCQueue f) where
    (.) = o
    id = tsingleton pure
