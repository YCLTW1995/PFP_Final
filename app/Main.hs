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

repeating :: [a] -> [[a]]
repeating xs = xs : repeating xs

create2D_array::( Num f)=> Array (Integer, Integer) (Maybe f)
create2D_array = listArray ((0,0), (9,9))  $ join $ ([replicate 10 (Just 40)] ++ 
                                               (take 8 (repeating (Just 40 : replicate 8 Nothing ++ [Just 40])))
                                                ++ [replicate 10 (Just 40)] )                    

main :: IO()
main = do
  -- printing 10000 elements cost 0.2 sec
  
  -- 2D code
  let b2 = create2D_array
  let bdiff2 = boundStep b2 (fmap (*0.1) . lap2)
  let a2 = (0,0) :. listArray ((0,0), (9,9)) (repeat $ Just 0)
  let l2 = execution bdiff2 a2
  let result = l2 !! 1234
  print $ result
  putStrLn "Main Function Done"
  {- Experiment on 1 2 4  8 core
2D
  -- size 100*100  iter 1234 : 
  -- size 10*10 iter 1234 :  
  
-}

  -- 1D code 
  {-}
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
