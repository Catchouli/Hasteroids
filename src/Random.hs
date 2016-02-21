module Random
    ( getRandom
    , getRandomR
    , getRandomPair
    , getRandomPairR1
    , getRandomPairR2
    ) where

import System.Random
import Control.Monad
import Control.Monad.State

getRandom :: (Monad m, Random a) => StateT StdGen m a
getRandom = state random

getRandomR :: (Monad m, Random a) => (a, a) -> StateT StdGen m a
getRandomR r = state $ randomR r

getRandomPair :: (Monad m, Random a) => StateT StdGen m (a, a)
getRandomPair = (,) <$> getRandom <*> getRandom

getRandomPairR1 :: (Monad m, Random a) => (a, a) -> StateT StdGen m (a, a)
getRandomPairR1 r = (,) <$> getRandomR r <*> getRandomR r

getRandomPairR2 :: (Monad m, Random a) => (a, a) -> (a, a) -> StateT StdGen m (a, a)
getRandomPairR2 r1 r2 = (,) <$> getRandomR r1 <*> getRandomR r2
