module StateExample where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import System.Random
import Data.Functor.Identity

data Die = One | Two | Three | Four | Five | Six
    deriving Show

intToDie :: Int -> Maybe Die
intToDie n = case n of
    1 -> Just One
    2 -> Just Two
    3 -> Just Three
    4 -> Just Four
    5 -> Just Five
    6 -> Just Six
    _ -> Nothing

unwrap :: Maybe a -> a
unwrap (Just v) = v

roll :: State StdGen Die
roll = state $ do
    (number, st) <- randomR (1, 6)
    return ((unwrap . intToDie) number, st)

roll' :: State StdGen Die
roll' = unwrap . intToDie <$> state (randomR (1, 6))

roll3 :: StateT StdGen Data.Functor.Identity.Identity (Die, Die, Die)
roll3 = liftA3 (,,) roll roll roll

roll3' :: State StdGen (Die, Die, Die)
roll3' = liftA3 (,,) roll' roll' roll'

nDie :: Int -> State StdGen [Die]
nDie = flip replicateM roll

