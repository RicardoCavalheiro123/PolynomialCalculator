module Test where

import qualified Main

import Test.QuickCheck
import MyPol (addPol, multiplyPol, multiplyMonPol, Polynomial, addPolynomial, Monomial, polynomialToString)

{-
data PolTest a = Polynomial a 

instance Arbitrary a => Arbitrary (Polynomial a) where
    arbitrary =
        sized arbitrarySizedPolynomial

arbitrarySizedPolynomial :: Arbitrary a => Int -> Gen (Polynomial a)
arbitrarySizedPolynomial m = do
    coef <- arbitrary
    power <- choose (0, 9999)
    return (Polynomial [(coef, [(var, power)])])
-}

prop :: Polynomial -> Polynomial -> Bool
prop a b = addPol (polynomialToString a) (polynomialToString b) == addPol (polynomialToString b) (polynomialToString a)
