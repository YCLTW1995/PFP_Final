{-# LANGUAGE TypeOperators #-}

import Lib_StrategyVersion as StrategyVersion
import Lib_RepaVersion as RepaVersion

-- Repa version array declaration
import qualified Data.Array.Repa as R

line :: Int -> RepaVersion.System R.U (R.Z R.:. Int) Double
line n = RepaVersion.System iv bounds ham where
  iv = R.fromFunction (R.Z R.:. (n + 2)) $ const 0
  bounds = pad (Just 1) $ R.fromFunction (R.Z R.:. n) $ const Nothing
  ham = pad (0 :: Double) . R.map (*0.1) . lap




main :: IO()
main = do
  -- Repa version
  simulateTo 10 (line 10) >>= print
  ---------------------------------------------------
  -- Strategy 2D code
  let b2 = StrategyVersion.create2D_array 
  let bdiff2 = StrategyVersion.boundStep b2 (fmap (*0.1) . StrategyVersion.lap2)
  let a2 = StrategyVersion.create2D_allzero 10
  let l2 = StrategyVersion.execution bdiff2 a2
  let result = l2 !! 1234
  print $ result
  putStrLn "Main Function Done"

-----------------------------------------------------
  -- Strategy 1D code 
  {-
  let b1 = listArray (0, 10000) (Just 40 : replicate 9999 Nothing ++ [Just 40])
  let bdiff1 = boundStep b1 (fmap (*0.1) . lap1)
  let a1 = 0 :. listArray (0, 10000) (replicate 10001 $ Just 0)
  let l1 = execution bdiff1 a1 

  let result = l1 !! 1234
  print $ result
  putStrLn "Main Function Done"
  -}
{- Experiment on 1 2 4  8 core
1D
  -- size 10000  iter 1234 : 14.974 6.819 5.6 6.091
  -- size 100 iter 1234 :    0.246  0.471
  
-} 

 
-- use "stack exec -- pfp-final-exe +RTS -N2 -ls" to records event 
