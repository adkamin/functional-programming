module RandomGen where

import Control.Monad
import System.Random
import RandomState

genRandIntegerIO :: (Integer,Integer) -> IO Integer
genRandIntegerIO (a,b) = do
  iost <- getStdGen
  let (i,st) = randomR (a,b) iost
  setStdGen st
  return i

genRandInteger :: (Integer,Integer) -> RandomState Integer
genRandInteger (a,b) = do
  rst <- get
  let (i,st) = randomR (a,b) rst
  put st
  return i

roll_2d6 :: RandomState Integer
roll_2d6 = do
  a <- genRandInteger (1,6)
  b <- genRandInteger (1,6)
  return (a+b)

safeR :: RandomState a -> IO a
safeR m = do
  state <- getStdGen
  let (x,state') = runState m state
  setStdGen state'
  return x

-- these definitions can be used to test your function a bit more thoroughly
randomN :: (Integer,Integer) -> Int -> StdGen -> [Integer]
randomN (a,b) n g = result
  where (result, _) = runState (replicateM n (genRandInteger (a,b))) g

testme :: [Integer]
testme = randomN (0,999) 100 (mkStdGen 42)
