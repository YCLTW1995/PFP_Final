module Lib
    ( lap,
      pad,
      Bound,
      Operator,
      System(System),
      simulateTo,
      simulateToSeq
    ) where

import Data.Maybe(fromMaybe)
import Control.Monad(guard)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed(Unbox)


type Operator r sh c = R.Array r sh c -> R.Array R.D sh c

reduce :: R.Shape sh => Int -> sh -> sh
reduce n = R.shapeOfList . map (subtract n) . R.listOfShape

tlist :: R.Shape sh => Int -> [sh]
tlist n = map (R.shapeOfList . take n) $ do l <- take n $ iterate (1:) $ 0 : repeat 1
                                            [l, map (2-) l]

lap :: (R.Shape sh, R.Source r c, Num c) => Operator r sh c
lap f = R.traverse f (reduce 2) llap where
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

step :: (R.Shape sh, R.Source r c, Num c) => Operator r sh c -> Operator r sh c
step t f = f R.+^ t f

boundStep :: (R.Shape sh, R.Source r c, Num c) => Bound sh c -> Operator r sh c -> Operator r sh c
boundStep b t f = R.zipWith fromMaybe (step t f) b


stepTo :: (R.Shape sh, Unbox c, Num c, Monad m) => Int -> Operator R.U sh c -> R.Array R.U sh c -> m (R.Array R.U sh c)
stepTo n h iv | n == 0 = return iv
              | otherwise = R.computeP (h iv) >>= stepTo (n-1) h

stepToSeq :: (R.Shape sh, Unbox c, Num c, Monad m) => Int -> Operator R.U sh c -> R.Array R.U sh c -> m (R.Array R.U sh c)
stepToSeq n h iv | n == 0 = return iv
                 | otherwise = stepToSeq (n-1) h $ R.computeS (h iv)


data System r sh c = System { iv :: R.Array R.D sh c, boundary :: Bound sh c, hamiltonian :: Operator r sh c }

simulateTo :: (R.Shape sh, Unbox c, Num c, Monad m) => Int -> System R.U sh c -> m (R.Array R.U sh c)
simulateTo n sys@(System iv b h) = R.computeP iv >>= stepTo n (boundStep b h)

simulateToSeq :: (R.Shape sh, Unbox c, Num c, Monad m) => Int -> System R.U sh c -> m (R.Array R.U sh c)
simulateToSeq n sys@(System iv b h) = stepToSeq n (boundStep b h) $ R.computeS iv

