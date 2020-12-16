import Lib

import qualified Data.Array.Repa as R

b1 = pad (Just 1) $ R.fromFunction (R.Z R.:. (8 :: Int)) $ const Nothing
bdiff1 = boundStep b1 (R.map (*0.1) . lap')
a1 = R.fromFunction (R.Z R.:. (10 :: Int)) $ const 0
l1 = simulate bdiff1 a1



b2 = pad (Just 1) $ R.fromFunction (R.Z R.:. (8 :: Int) R.:. (8 :: Int)) $ const Nothing
bdiff2 = boundStep b2 (R.map (*0.1) . lap')
a2 = R.fromFunction (R.Z R.:. (10 :: Int) R.:. (10 :: Int)) $ const 0
l2 = simulate bdiff2 a2


main :: IO()
main = putStrLn $ show (l1 !! 10)



{-
-- example 

b1 = listArray (0, 10) (Just 1 : replicate 9 Nothing ++ [Just 1])
bdiff1 = boundStep b1 (fmap (*0.1) . lap1)
a1 = 0 :. listArray (0, 10) (replicate 11 $ Just 0)
l1 = map (map rmJ . toList) $ simulate bdiff1 a1
    where rmJ Nothing = undefined
          rmJ (Just n) = n



b2 = listArray ((0,0), (5,5)) $ join $ [[e,e,e,e,e,e],
                                        [e,o,o,o,o,e],
                                        [e,o,o,o,o,e],
                                        [e,o,o,o,o,e],
                                        [e,o,o,o,o,e],
                                        [e,e,e,e,e,e]]
    where e = Just 1
          o = Nothing
bdiff2 = boundStep b2 (fmap (*0.1) . lap2)
a2 = (0,0) :. listArray ((0,0), (5,5)) (repeat $ Just 0)
l2 = map (fmap rmJ) $ simulate (boundStep b2 bdiff2) a2
    where rmJ Nothing = undefined
          rmJ (Just n) = n

 -}