module Tests where

-- GHC
import System.Exit
import Data.Char (isPunctuation)
-- External
import Test.HUnit
-- import Test.QuickCheck

-- Lib
import Problem3

p3a, p3b :: Test

a, b, c :: Formula
a = variable "a"
b = variable "b"
c = variable "c"

allDistinct          :: [Formula] -> Bool
allDistinct []       = True
allDistinct (f : fs) = distinct f fs && allDistinct fs
  where
    distinct f [] = True
    distinct f1 (f2 : fs) = f1 /= f2 && distinct f1 fs

p3a = test [
    -- smart constructors have
    -- no runtime errors
   a        @?= a,
   neg a    @?= neg a,
   conj a b @?= conj a b,
   disj a b @?= disj a b,
   allDistinct [
       a,
       b,
       neg (neg a),
       neg a,
       neg b,
       conj a b,
       conj b a,
       disj a b,
       disj b a
       ] @? "Smart constructors construct distinct terms"
   ]

repeatNeg :: Int -> Formula -> Formula
repeatNeg 0 f = f
repeatNeg n f = neg (repeatNeg (n - 1) f)

con = foldr conj a [a, b, c]
dis = foldr disj a [c, b, a]

unnecessarilyComplicatedFormula :: (Formula -> Formula) -> Formula
unnecessarilyComplicatedFormula f = foldr disj con (map f [con, dis, con, dis])

p3b = test [
  -- nnf fixed-points
  nnf a                @?= a,
  nnf (neg a)          @?= neg a,
  nnf (conj a b)       @?= conj a b,
  nnf (disj a b)       @?= disj a b,
  -- asserted logical equivs.
  nnf (neg (neg a))    @?= a,
  nnf (neg (disj a b)) @?= conj (neg a) (neg b),
  nnf (neg (conj a b)) @?= disj (neg a) (neg b),
  -- nested negation
  nnf (repeatNeg 6 a)           @?= a,
  nnf (repeatNeg 7 a)           @?= neg a,
  nnf (repeatNeg 8 (disj a b))  @?= disj a b,
  nnf (repeatNeg 9 (disj a b))  @?= conj (neg a) (neg b),
  nnf (repeatNeg 10 (conj a b)) @?= conj a b,
  nnf (repeatNeg 11 (conj a b)) @?= disj (neg a) (neg b),
  -- trinary ops
  nnf (neg (conj (conj a b) c)) @?= disj (disj (neg a) (neg b)) (neg c),
  nnf (neg (disj (disj a b) c)) @?= conj (conj (neg a) (neg b)) (neg c),
  nnf (neg (disj (conj a b) c)) @?= conj (disj (neg a) (neg b)) (neg c),
  -- just for fun
  nnf (unnecessarilyComplicatedFormula (neg . neg)) @?= nnf (unnecessarilyComplicatedFormula id)
  ]

tests = test [p3a, p3b]

main :: IO ()
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)

  
