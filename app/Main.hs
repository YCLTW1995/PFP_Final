{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

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


-- pointed arrays; we might want to replace Array with something like CArray for a speedup
infixr 5 :.
data PArray i f = i :. Array i f
    deriving (Show, Foldable, Functor)

instance Ix i => Comonad (PArray i) where -- possibility of parallelism here
    extract (j:.a) = a ! j
    
    extend f (i :. a)  = i :. my_listArray (bounds a) (f . (:.a) <$> indices a)
    
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


lap2 :: Num f => LocOp (Int, Int) f
lap2 pa@((x,y):.a) = do n <- join $ a !? (x, y + 1)
                        s <- join $ a !? (x, y - 1)
                        e <- join $ a !? (x + 1, y)
                        w <- join $ a !? (x - 1, y)
                        c <- extract pa
                        return $ n + s + e + w - 4 * c

lap3 :: Num f => LocOp (Int, Int, Int) f
lap3 = undefined




type Bound i f = Array i (Maybe f)

boundStep :: (Ix i, Num f) => Bound i f -> LocOp i f -> LocOp i f
boundStep b d pa@(j:._) = mplus (b ! j) $ (+) <$> extract pa <*> d pa


simulate :: (Ix i, Num f) => LocOp i f -> PArray i (Maybe f)  -> [PArray i (Maybe f)]
simulate d pa = pa : map (extend d) (simulate d pa) 


execution :: (Ix i, Num f) =>  LocOp i f -> PArray i (Maybe f)-> [[f]] 
execution bdiff1 a1 = map (map rmJ . toList) $ simulate bdiff1 a1 
    where rmJ Nothing = undefined
          rmJ (Just n) = n

main :: IO()
main = do
  let b1 = listArray (0, 10) (Just 1 : replicate 9 Nothing ++ [Just 1])
  let bdiff1 = boundStep b1 (fmap (*0.1) . lap1)
  let a1 = 0 :. listArray (0, 10) (replicate 11 $ Just 0)
  let l1 = execution bdiff1 a1 

  let result = l1 !! 12
  print $ result 
  putStrLn "Main Function Done"

-- use "stack exec -- pfp-final-exe +RTS -N2 -ls" to records event 

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