{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}

import Test.Hspec hiding (Example)
import Data.GradientDescent
import GHC.Generics

-- A general parabola following the formula
-- f(x) = ax² + bx + c
data Parabola = Parabola {
    x :: Parameter,
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

-- A bidimensional parabola made from two parabolas
-- f(x, y) = ax² + bx + c + dx² + ex + f
data BidimensionalParabola = BidimensionalParabola {
        parabola1 :: Parabola
    ,   parabola2 :: Parabola
    } deriving (Show, Eq, Generic, ExtractParameters, InjectParameters)

instance GradientDescent BidimensionalParabola where
    f (BidimensionalParabola a b) = f a + f b

getBidimensionalParabolaXY :: BidimensionalParabola -> (Double, Double)
getBidimensionalParabolaXY (BidimensionalParabola a b) = (unParameter $ x a, unParameter $ x b)

main :: IO ()
main = hspec $ do
    let solvedParabola = Parabola (-0.5) 1 1 1
    let unsolvedParabola = Parabola (-4.5) 1 1 1
    describe "Parabola" $ do
        it "formula is correct" $
            f solvedParabola `shouldBe` 0.75
            
        it "descent is accurate after 20 iterations" $
            getParabolaX (last $ take 20 $ descent 1 unsolvedParabola) `shouldSatisfy` (\x -> x ~= getParabolaX solvedParabola)
        describe "ExtractParameters" $ do
            it "extractParameters1" $
                gExtractParameters (from unsolvedParabola) `shouldBe` [-4.5]
            it "extractParameters" $
                extractParameters unsolvedParabola `shouldBe` [-4.5]
        describe "injectParameters" $
            it "injectParameters" $
                injectParameters unsolvedParabola [6] `shouldBe` (Parabola 6 1 1 1, [])

    describe "BidimensionalParabola" $ do
        describe "ExtractParameters" $
            it "extractParameters" $
                extractParameters (BidimensionalParabola unsolvedParabola solvedParabola) `shouldBe` [-4.5, -0.5]
        describe "injectParameters" $
            it "injectParameters" $
                injectParameters (BidimensionalParabola unsolvedParabola unsolvedParabola) [1, 2]
                  `shouldBe`
                  (BidimensionalParabola (Parabola 1 1 1 1) (Parabola 2 1 1 1), [])
        it "descent is accurate after 20 iterations" $ do
            let bidip = BidimensionalParabola unsolvedParabola unsolvedParabola
            getBidimensionalParabolaXY (last $ take 20 $ descent 1 bidip)
              `shouldSatisfy`
              (\(x, y) -> x ~= (-0.5) && y ~= (-0.5))

-- Floating point "Almost Equal" operation 
(~=) :: Double -> Double -> Bool
(~=) x y = (x + epsilon) > y && (x - epsilon) < y
  where
    epsilon = 0.000001


