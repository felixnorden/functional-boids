module Vector where

import Control.Applicative (pure)
import Data.Boolean (otherwise)
import Data.Eq (class Eq, (==))
import Data.Foldable (and, sum)
import Data.Function (($))
import Data.Functor ((<$>), map)
import Data.Int (toNumber)
import Data.List (List(..), foldr, head, length, zipWith, (:))
import Data.Maybe (Maybe(..))
import Data.Ord ((<=))
import Data.Ring ((+), (*), (-), add, mul, negate, class Ring)
import Data.Semigroup ((<>))
import Data.Semiring (class Semiring, zero, one)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Math (round, sqrt)
import Prelude ((/), bind, (<<<))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, choose, listOf)
import Type.Data.Boolean (kind Boolean)

data Vector a = V (List a)

vector :: forall a. Ring a => List a -> Vector a
vector (a:as) = V $ a:as
vector _      = V Nil

zeroV :: forall a. Semiring a => Int -> Vector a
zeroV dim = V $ unfoldr nextZero dim
  where   nextZero :: Int -> Maybe (Tuple a Int)
          nextZero n | n <= 0     = Nothing
                     | otherwise  = Just (Tuple zero (n-1))

instance showVector :: (Show a) => Show (Vector a) where
  show (V nums) = "(" <> components nums <> ")" where
    components (x:Nil)  = show x    
    components (x:xs)   = show x <> ", " <> (components xs)
    components (_)      = ""

instance eqVector :: Eq a => Eq (Vector a) where
  eq (V xs) (V ys) = and terms
    where terms = zipWith (==) xs ys 

addV :: forall a. Semiring a => Vector a -> Vector a -> Vector a
addV (V as) (V bs) = V $ zipWith (+) as bs

subV :: forall a. Ring a => Vector a -> Vector a -> Vector a
subV a b = addV a $ scalarMul (negate one) b

-- mulV :: Vector -> Vector -> Vector
-- mulV (V as) (V bs) = V (zipWith (*) as bs)

scalarMul :: forall a. Semiring a => a -> Vector a -> Vector a
scalarMul s (V bs) = V (mul s <$> bs)

absV :: Vector Number -> Number
absV (V xs) = sqrt $ foldr (\x -> add (x*x)) zero xs

dimensionsV :: forall a. Vector a -> Int
dimensionsV (V xs) = length xs

sumV :: List (Vector Number) -> Vector Number
sumV vecs = foldr addV (zeroV dims) vecs
  where vec  = head vecs
        dims = case vec of
            Just xs	    -> dimensionsV xs
            Nothing     -> 0

averageV :: List (Vector Number) -> Vector Number
averageV vecs = scalarMul scalar totalSum
  where scalar = 1.0 / (toNumber $ length vecs)
        totalSum = sumV vecs

-- | Properties and tests

rVector :: Gen (Vector Number)
rVector = do list <- listOf 3 $ choose 0.0 10.0
             pure $ V list

instance arbVector :: Arbitrary (Vector Number) where
  arbitrary = rVector

prop_absV :: Vector Number -> Boolean
prop_absV (V xs) = (round $ absV (V xs)) == (round <<< sqrt <<< sum <<< map (\x -> x*x) $ xs)

prop_addV :: Vector Number -> Vector Number -> Boolean
prop_addV (V as) (V bs) = (addV (V as) (V bs)) == V (sumList as bs)
  where sumList (x:xs) (y:ys) = (x+y) : sumList xs ys
        sumList (_)    (_)    = Nil