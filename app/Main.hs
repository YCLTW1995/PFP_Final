import Lib_StrategyVersion as StrategyVersion
import Lib_RepaVersion as RepaVersion

-- Repa version array declaration
import qualified Data.Array.Repa as R

b1 = RepaVersion.pad (Just 1) $ R.fromFunction (R.Z R.:. (8 :: Int)) $ const Nothing
bdiff1 = RepaVersion.boundStep b1 (R.map (*0.1) . RepaVersion.lap')
a1 = R.fromFunction (R.Z R.:. (10 :: Int)) $ const 0
l1 = RepaVersion.simulate bdiff1 a1



b2 = RepaVersion.pad (Just 1) $ R.fromFunction (R.Z R.:. (8 :: Int) R.:. (8 :: Int)) $ const Nothing
bdiff2 = RepaVersion.boundStep b2 (R.map (*0.1) . RepaVersion.lap')
a2 = R.fromFunction (R.Z R.:. (10 :: Int) R.:. (10 :: Int)) $ const 0
l2 = RepaVersion.simulate bdiff2 a2


main :: IO()
main = do
  -- Repa version
  putStrLn $ show (l1 !! 10)
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
