{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

import Test.Hspec hiding (Example)
import Data.GradientDescent
import GHC.Generics

-- A general parabola following the formula
-- f(x) = ax² + bx + c
data Parabola = Parabola {
    x :: Parameter Double,
    a :: Double,
    b :: Double,
    c :: Double
  } deriving (Show, Eq, Generic, ExtractParameters, InjectParameters)

getParabolaX :: Parabola -> Double
getParabolaX p = unParameter $ x p


instance GradientDescent Parabola where
    f p@Parabola { a, b, c} = a * x * x + b * x + c
        where
            x = getParabolaX p

-- f(x, y) = ax² + bx + c + dx² + ex + f
data BidimensionalParabola = BidimensionalParabola {
        parabola1 :: Parabola
    ,   parabola2 :: Parabola
    } deriving (Show, Eq, Generic, ExtractParameters, InjectParameters)

main :: IO ()
main = hspec $ do
    describe "Parabola" $ do
        it "formula is correct" $ do
            f (Parabola (-0.5) 1 1 1) `shouldBe` 0.75
            
        it "descent is accurate after 20 iterations" $ do
            getParabolaX (last $ take 20 $ descent 1 (Parabola (-4.5) 1 1 1)) `shouldSatisfy` (\x -> x< (-0.499999999) && x> (-0.50001))
        describe "ExtractParameters" $ do
            it "extractParameters1" $ do
                (extractParameters1 $ from $ (Parabola (-4.5) 1 1 1)) `shouldBe` [-4.5]
            it "extractParameters" $ do
                (extractParameters (Parabola (-4.5) 1 1 1)) `shouldBe` [-4.5]
        describe "injectParameters" $ do
            it "injectParameters" $ do
                injectParameters (Parabola (-4.5) 1 1 1) [6] `shouldBe` (Parabola (6) 1 1 1)
