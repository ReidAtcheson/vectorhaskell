{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Foldable
import           System.Environment

data LinearSpace a b where
  Vector :: a -> LinearSpace a b
  Sum    :: LinearSpace a b -> LinearSpace a b -> LinearSpace a b
  Scale  :: b -> LinearSpace a b -> LinearSpace a b

data LinearSpaceOpt a b where
  LinearComb  :: [(b,LinearSpace a b)] -> LinearSpaceOpt a b

{-
showVectorExpr :: (Show a,Show b) => LinearSpace a b -> String
showVectorExpr (Vector a) = "(Array " ++ (show a) ++  ")"
showVectorExpr (Sum a1 a2) = "(" ++ (showVectorExpr a1) ++ "+" ++ (showVectorExpr a2) ++ ")"
showVectorExpr (Scale b a1) = (show b) ++ "*" ++ (showVectorExpr a1)
-}

vectorExpr2lc :: (Num b) => LinearSpace a b -> [(b,LinearSpace a b)]
vectorExpr2lc (Vector x)  = [(1,Vector x)]
vectorExpr2lc (Scale u x) = map (\z->(u* fst z,snd z)) (vectorExpr2lc x)
vectorExpr2lc (Sum x y)   = vectorExpr2lc x ++ vectorExpr2lc y

optVectorExpr :: (Num b) => LinearSpace a b -> LinearSpaceOpt a b
optVectorExpr x = LinearComb (vectorExpr2lc x)


evalUArrayExpr :: LinearSpace (UArray Int Int) Int -> LinearSpace (UArray Int Int) Int
evalUArrayExpr (Vector x) = Vector x
evalUArrayExpr (Sum (Vector x) (Vector y))  = Vector z where
  z = listArray (1,n) [(x ! i) + (y ! i) | i <- [1..n]] where
    n = snd $ bounds x
evalUArrayExpr (Sum x (Vector y)) = evalUArrayExpr (Sum (evalUArrayExpr x) (Vector y))
evalUArrayExpr (Sum (Vector y) x) = evalUArrayExpr (Sum x (Vector y))
evalUArrayExpr (Sum x y) = evalUArrayExpr (Sum (evalUArrayExpr x) (evalUArrayExpr y))
evalUArrayExpr (Scale y (Vector x)) = Vector z where
  z = listArray (1,n) [(x ! i) * y | i<-[1..n]] where
    n = snd $ bounds x
evalUArrayExpr (Scale y x) = evalUArrayExpr (Scale y (evalUArrayExpr x))

getUArray :: forall t t1. LinearSpace t t1 -> t
getUArray (Vector x) = x
getUArray _ = error "bad"
make :: [Int] -> ST s (STUArray s Int Int)
make xs = newListArray (1, length xs) xs

evalUArrayExprOpt :: LinearSpaceOpt (UArray Int Int) Int -> LinearSpace (UArray Int Int) Int
evalUArrayExprOpt (LinearComb x) = Vector y where
  y=runSTUArray $ do
      let n= (snd  . bounds . getUArray . snd  . head) x
      tmp<-make (replicate n 0)
      let vs = map (\s -> (fst s,getUArray (snd s))) x
      forM_ [1..n] $ \i1 ->
        forM_ vs $ \i2 -> do
            val1<-readArray tmp i1
            let val2 = fst i2 * snd i2 ! i1
            writeArray tmp i1 (val1+val2)
      return tmp





main :: IO ()
main = do
  args<-getArgs
  let arrval = ( read . head) args :: Int
  let n = read (args !! 1)::Int
  let nterms =  read  (args !! 2):: Int
  let mode= args !! 3
  let x = listArray (1,n) [arrval+i | i<-[1..n]] :: UArray Int Int
  let y = listArray (1,n) [arrval+i | i<-[1..n]] :: UArray Int Int
  let zeros = listArray (1,n) (replicate n 0) :: UArray Int Int
  let vx = Vector x
  let vy = Vector y
  let zs = Vector zeros
  let vxs= [Scale i vx | i<-[1..nterms]]
  let vys= [Scale (i-1) vy | i<-[1..nterms]]
  let vvs= zipWith Sum vxs vys
  let vz = foldr Sum zs vvs
  let h  = evalUArrayExpr vz
  let hopt=evalUArrayExprOpt (optVectorExpr vz)
  let out= getUArray (if mode=="opt" then hopt else h)
  let val= foldl' (+) 0 (elems out)
  print val
