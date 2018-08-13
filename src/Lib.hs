{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import Numeric.Natural
import GHC.Generics
someFunc :: IO ()
someFunc = do
    sweep (Example (Parameter 0) 1 1 1)
    multiStepDescent 1 100 (Example (Parameter 48) 1 1 1)
    print $ extractParameters $ from $ (Example (Parameter 48) 1 1 1)

data Parameter a = Parameter a deriving (Show, Eq)
unParameter (Parameter x) = x

data Example = Example {
    ex :: Parameter Double,
    ea :: Double,
    eb :: Double,
    ec :: Double
  } deriving (Show, Eq, Generic)

fx :: Example -> Double
fx Example { ex, ea, eb, ec} = ea * x * x + eb * x + ec
    where
        x = unParameter ex

sweep :: Example -> IO ()
sweep e = mapM_ p vals
    where
        vals = (\v -> e {ex=Parameter v}) <$> [-10.0, -9.9 .. 10.0]
        p x = print (unParameter $ ex x, fx x)

delta :: Example -> Double
delta e = ((fx e) - (fx e2))/distance
    where
        e2 = e {ex=Parameter $ unParameter (ex e) + distance}
        distance = 0.00001

gradientDescent :: Double -> Example -> Example
gradientDescent increment e = e {ex=Parameter $ unParameter (ex e) + distance}
    where
        distance = delta e * increment

multiStepDescent :: Double -> Int -> Example -> IO ()
multiStepDescent _ 0 _ = pure ()
multiStepDescent increment iters e = do
                           pr e
                           multiStepDescent (increment * 0.95) (iters - 1) next
    where
        pr e = print $ ("x", (unParameter . ex) e, "y", fx e, "dy/dx", delta e)
        next = gradientDescent increment e

class ExtractParameters a where
  -- | Return number of constuctor fields for a value.
  extractParameters :: a -> [Double]

instance ExtractParameters (V1 p) where
  extractParameters _ = []

instance ExtractParameters (U1 p) where
  extractParameters _ = []

instance ExtractParameters (K1 i (Parameter Double) p) where
  extractParameters (K1 (Parameter x)) = [x]

instance ExtractParameters (K1 i Double p) where
  extractParameters _ = []

instance ExtractParameters (f p) => ExtractParameters (M1 i c f p) where
  extractParameters (M1 x) = extractParameters x

instance (ExtractParameters (a p), ExtractParameters (b p)) => ExtractParameters ((a :+: b) p) where
  extractParameters (L1 x) = extractParameters x
  extractParameters (R1 x) = extractParameters x

instance (ExtractParameters (a p), ExtractParameters (b p)) => ExtractParameters ((a :*: b) p) where
  extractParameters (a :*: b) = extractParameters a ++ extractParameters b
