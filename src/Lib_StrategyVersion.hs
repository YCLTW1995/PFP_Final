{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Lib_StrategyVersion
    ( 
      parLIST,
      parMAP,
      my_listArray,
      zipwith_seq,
      zipwith_par,
      lap1,
      lap2,
      boundStep,
      simulate,
      execution,
      repeating,
      create2D_array,
      create2D_allzero
    ) where

import Control.Monad(join, mplus)
import Control.Comonad -- hackage
import Data.Array
import Control.Parallel.Strategies
import Data.Foldable(toList)
import Control.Monad.Par
import Control.DeepSeq
        
(!?) :: Ix i => Array i e -> i -> Maybe e
a !? j | bounds a `inRange` j = Just (a ! j)
       | otherwise            = Nothing
    

infixr 5 :.
data PArray i f = i :. Array i f
    deriving (Show, Foldable, Functor)
-- pointed arrays; we might want to replace Array with something like CArray for a speedup
instance Ix i => Comonad (PArray i) where -- possibility of parallelism here
    extract (j:.a) = a ! j
    extend f (i :. a)  = i :. my_listArray (bounds a) (fmap (f . flip (:.) a) (range bds) `using` parLIST rseq)
      where bds = bounds a
    --f `extend` (j:.a) = j :. listArray bds (fmap (f . flip (:.) a) (range bds) `using` parLIST rseq)
        --where bds = bounds a
    -- http://blog.sigfpe.com/2008/03/comonadic-arrays.html
       -- extend :: (w a -> b ) -> w a -> w b


parLIST:: Strategy a -> Strategy [a]
parLIST s = parMAP (rparWith s)

parMAP :: Strategy a -> Strategy [a]
parMAP _ [] = return []
parMAP f (a:as) = do 
  b <-  (f a)
  bs <- parMAP f as 
  return (b:bs)


my_listArray             :: (Ix a) => (a,a) -> [b] -> Array a b
my_listArray b vs        =  array b (runEval ( zipwith_par (\ a b -> (a,b)) (range b) vs))

zipwith_seq::(a -> b -> c) -> [a] -> [b] -> [c]
zipwith_seq f (x:xs) (y:ys) = (f x y) : (zipwith_seq f xs ys)
zipwith_seq _ _ _ = []

zipwith_par :: (a -> b -> c) -> [a] -> [b] -> Eval [c]
zipwith_par _ [] [] = return []
zipwith_par f (a:as) (b:bs) = do
  c <- rpar (f a b)
  cs <- zipwith_par f as bs
  return (c:cs) 

type LocOp i f = PArray i (Maybe f) -> Maybe f

-- I think these can be combined into one using the graph laplacian,
-- which would also allow for more parallelism

lap1 :: Num f => LocOp Int f
lap1 pa @ (i :. a) = do l <- join $ a !? (i - 1)
                        r <- join $ a !? (i + 1)
                        c <- extract pa
                        return $ l + r - 2 * c


lap2 :: Num f => LocOp (Integer, Integer) f
lap2 pa@((x,y):.a) = do n <- join $ a !? (x, y + 1)
                        s <- join $ a !? (x, y - 1)
                        e <- join $ a !? (x + 1, y)
                        w <- join $ a !? (x - 1, y)
                        c <- extract pa
                        return $ n + s + e + w - 4 * c



type Bound i f = Array i (Maybe f)

boundStep :: (Ix i, Num f) => Bound i f -> LocOp i f -> LocOp i f
boundStep b d pa@(j:._) = mplus (b ! j) $ (+) <$> extract pa <*> d pa


simulate :: (Ix i, Num f) => LocOp i f -> PArray i (Maybe f)  -> [PArray i (Maybe f)]
simulate d pa = pa : map (extend d) (simulate d pa) 


execution :: (Ix i, Num f) =>  LocOp i f -> PArray i (Maybe f)-> [[f]] 
execution bdiff1 a1 = map (map rmJ . toList) $ simulate bdiff1 a1 
    where rmJ Nothing = undefined
          rmJ (Just n) = n

repeating :: [a] -> [[a]]
repeating xs = xs : repeating xs

create2D_array::( Num f)=> Array (Integer, Integer) (Maybe f)
create2D_array = listArray ((0,0), (9,9))  $ join $ ([replicate 10 (Just 40)] ++ 
                                               (take 8 (repeating (Just 40 : replicate 8 Nothing ++ [Just 40])))
                                               ++ [replicate 10 (Just 40)] )

create2D_allzero::( Num f)=> Integer -> PArray (Integer, Integer) (Maybe f)
create2D_allzero size = (0,0) :. listArray ((0,0), ((size-1),(size-1))) (repeat $ Just 0)