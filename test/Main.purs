module Test.Main where

import Prelude (($))

import Test.QuickCheck (quickCheck, (===), Result)

import Data.Multinumber (fromFoldable, toArray)
import Data.Array (sort)

-- | for any array
toFromId :: Array Int -> Result
toFromId xs = sort (toArray $ fromFoldable xs) === sort xs

-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Random (RANDOM)
-- import Control.Monad.Eff.Console (CONSOLE)
-- import Prelude (Unit)
-- import Control.Monad.Eff.Exception (EXCEPTION)

-- main :: forall e. Eff (con :: CONSOLE, rnd :: RANDOM, err :: EXCEPTION| e)
main = do quickCheck toFromId
