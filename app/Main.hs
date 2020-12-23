{-# LANGUAGE TypeOperators #-}

import Lib
import qualified Data.Array.Repa as R
import System.Environment as S

line :: Int -> System R.U (R.Z R.:. Int) Double
line n = System iv bounds ham where
  iv = R.fromFunction (R.Z R.:. (n + 2)) $ const 0
  bounds = pad (Just 1) $ R.fromFunction (R.Z R.:. n) $ const Nothing
  ham = pad (0 :: Double) . R.map (*0.1) . lap

square :: Int -> System R.U (R.Z R.:. Int R.:. Int) Double
square n = System iv bounds ham where
  iv = R.fromFunction (R.Z R.:. (n + 2) R.:. (n + 2)) $ const 0
  bounds = pad (Just 1) $ R.fromFunction (R.Z R.:. n R.:. n) $ const Nothing
  ham = pad (0 :: Double) . R.map (*0.1) . lap

cube :: Int -> System R.U (R.Z R.:. Int R.:. Int R.:. Int) Double
cube n = System iv bounds ham where
  iv = R.fromFunction (R.Z R.:. (n + 2) R.:. (n + 2) R.:. (n + 2)) $ const 0
  bounds = pad (Just 1) $ R.fromFunction (R.Z R.:. n R.:. n R.:. n) $ const Nothing
  ham = pad (0 :: Double) . R.map (*0.1) . lap

weirdLine :: Int -> System R.U (R.Z R.:. Int) Double
weirdLine n = System iv bounds ham where
  System iv bounds _ = line n
  ham = pad (0 :: Double) . R.map (\x -> x * x * 0.1) . lap

-- yes, this main function is ugly; I couldnt figure out how to separate it into two case statements because of the type polymorphism
main :: IO ()
main = do [par, its, shape, size] <- S.getArgs
          case (par, shape) of ("par", "line") -> simulateTo (read its) (line $ read size) >>= print
                               ("seq", "line") -> simulateToSeq (read its) (line $ read size) >>= print
                               ("par", "square") -> simulateTo (read its) (square $ read size) >>= print
                               ("seq", "square") -> simulateToSeq (read its) (square $ read size) >>= print
                               ("par", "cube") -> simulateTo (read its) (cube $ read size) >>= print
                               ("seq", "cube") -> simulateToSeq (read its) (cube $ read size) >>= print
                               ("par", "weird") -> simulateTo (read its) (weirdLine $ read size) >>= print
                               ("seq", "weird") -> simulateToSeq (read its) (cube $ read size) >>= print
