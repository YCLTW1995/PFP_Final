module Lib
    ( lap,
      lap',
      pad,
      Bound,
      Operator,
      step,
      boundStep,
      simulate
    ) where

import Data.Maybe(fromMaybe)
import Control.Monad(guard)
import qualified Data.Array.Repa as R


reduce :: R.Shape sh => Int -> sh -> sh
reduce n = R.shapeOfList . map (subtract n) . R.listOfShape

tlist :: R.Shape sh => Int -> [sh]
tlist n = map (R.shapeOfList . take n) $ do l <- take n $ iterate (1:) $ 0 : repeat 1
                                            [l, map (2-) l]

lap :: (R.Shape sh, R.Source r c, Num c) => R.Array r sh c -> R.Array R.D sh c
lap f = foldr ((R.+^) . shift) (R.map (* nrk) $ shift R.unitDim) $ tlist rk where
    outsh = reduce 2 $ R.extent f
    rk = R.rank outsh
    nrk = fromIntegral $ -2 * rk
    shift s = R.extract s outsh f

lap' :: (R.Shape sh, R.Source r c, Num c) => R.Array r sh c -> R.Array R.D sh c
lap' f = R.traverse f (reduce 2) llap where
    rk = R.rank $ R.extent f
    llap g p = nbhds g p - 2 * g (R.addDim p R.unitDim) * fromIntegral rk
    nbhds g p = sum $ map (g . R.addDim p) $ tlist rk





pad :: (R.Shape sh, R.Source r c) => c -> R.Array r sh c -> R.Array R.D sh c
pad p f = R.backpermuteDft padding checkReduce f where
    dim = R.extent f
    padding = R.fromFunction (dim `R.addDim` R.unitDim `R.addDim` R.unitDim) $ const p
    checkReduce s = do let r = reduce 1 s
                       guard $ R.inShape dim r
                       return r



type Bound sh c = R.Array R.D sh (Maybe c)
type Operator r sh c = R.Array r sh c -> R.Array R.D sh c

step :: (R.Shape sh, R.Source r c, Num c) => Operator r sh c -> Operator r sh c
step t f = f R.+^ pad 0 (t f)

boundStep :: (R.Shape sh, R.Source r c, Num c) => Bound sh c -> Operator r sh c -> Operator r sh c
boundStep b t f = R.zipWith fromMaybe (step t f) b

simulate :: R.Shape sh => Operator R.U sh Float -> R.Array R.D sh Float -> [R.Array R.U sh Float]
simulate t f = R.computeP f ++ do a <- simulate t f
                                  R.computeP $ t a