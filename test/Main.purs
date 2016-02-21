module Test.Main where

import Prelude ((+), (==))

import Test.QuickCheck (quickCheck, (===))

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console

-- obviously no real tests yet
main = do quickCheck \n -> n + 1 === 2 + n
