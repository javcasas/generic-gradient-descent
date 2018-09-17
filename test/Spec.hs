{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

import Test.Hspec hiding (Example)
--import Lib hiding (Example(..), fx)
import Data.GradientDescent
import GHC.Generics

data Example = Example {
    ex :: Parameter Double,
    ea :: Double,
    eb :: Double,
    ec :: Double
  } deriving (Show, Eq, Generic, ExtractParameters, InjectParameters)

getX :: Example -> Double
getX x = unParameter $ ex x


instance GradientDescent Example where
    --type DeltaRep Example = Double
    f Example { ex, ea, eb, ec} = ea * x * x + eb * x + ec
        where
            x = unParameter ex
    {-step l a = applyDelta l (delta a) a
        where
            delta Example {ex, ea, eb, ec} = ea * 2 * x + eb
                where
                    x = unParameter ex
            applyDelta step dx (Example { ex, ea, eb, ec }) = Example {ea, eb, ec, ex=tt }
                where
                    tt = Parameter $ (unParameter ex) - (step * dx)
-}

main :: IO ()
main = hspec $ do
    describe "Example" $ do
        it "Case 2" $ do
            f (Example (-0.5) 1 1 1) `shouldBe` 0.75
            
        it "Case 3" $ do
            getX (last $ take 20 $ descent 1 (Example (-4.5) 1 1 1)) `shouldSatisfy` (\x -> x< (-0.499999999) && x> (-0.50001))
    describe "ExtractParameters" $ do
        it "extractParameters1" $ do
            (extractParameters1 $ from $ (Example (-4.5) 1 1 1)) `shouldBe` [-4.5]
        it "extractParameters" $ do
            (extractParameters (Example (-4.5) 1 1 1)) `shouldBe` [-4.5]
    describe "injectParameters" $ do
        it "injectParameters" $ do
            injectParameters (Example (-4.5) 1 1 1) [6] `shouldBe` (Example (Parameter (6)) 1 1 1)
