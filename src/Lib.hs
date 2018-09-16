{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Lib where

import Numeric.Natural
import GHC.Generics
someFunc :: IO ()
someFunc = do
    sweep (Example (Parameter 0) 1 1 1)
    multiStepDescent 1 100 (Example (Parameter 48) 1 1 1)
    print $ extractParameters $ from $ (Example (Parameter 48) 1 1 1)

newtype Parameter a = Parameter { unParameter :: a } deriving (Show, Eq, Num)

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

delta2 :: Example -> Double
delta2 e = ((fx e) - (fx e2))/distance
    where
        e2 = e {ex=Parameter $ unParameter (ex e) + distance}
        distance = 0.00001

gradientDescent :: Double -> Example -> Example
gradientDescent increment e = e {ex=Parameter $ unParameter (ex e) + distance}
    where
        distance = delta2 e * increment

multiStepDescent :: Double -> Int -> Example -> IO ()
multiStepDescent _ 0 _ = pure ()
multiStepDescent increment iters e = do
                           pr e
                           multiStepDescent (increment * 0.95) (iters - 1) next
    where
        pr e = print $ ("x", (unParameter . ex) e, "y", fx e, "dy/dx", delta2 e)
        next = gradientDescent increment e

newtype Deltas a = Deltas a deriving (Show, Eq)
class GradientDescent a where
    type DeltaRep a :: *
    
    delta :: a -> DeltaRep a
    applyDelta :: Double -> DeltaRep a -> a -> a
    f :: a -> Double

descent :: (GradientDescent a) => Double -> a -> [a]
descent increment initial = 
    let d = delta initial in
    let applied = applyDelta increment d initial in
    applied : descent (increment * 0.95) applied
    

class ExtractParameters a where
  -- | Return number of constuctor fields for a value.
  extractParameters :: a -> [Double]
  default extractParameters :: (Generic a, ExtractParameters (Rep a)) => a -> [Double]
  extractParameters x = extractParameters $ from x

class ExtractParameters1 f where
  -- | Return number of constuctor fields for a value.
  extractParameters1 :: f p -> [Double]

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



class InjectParameters a where
  -- | Return number of constuctor fields for a value.
  injectParameters :: a -> [Double] -> (a, [Double])

instance InjectParameters (V1 p) where
  injectParameters x p = (x, p)

instance InjectParameters (U1 p) where
  injectParameters x p = (x, p)

instance InjectParameters (K1 i (Parameter Double) p) where
  injectParameters (K1 (Parameter x)) (h:t) = (K1 (Parameter h), t)

instance InjectParameters (K1 i Double p) where
  injectParameters x p = (x, p)

instance InjectParameters (f p) => InjectParameters (M1 i c f p) where
  injectParameters (M1 x) p = (M1 x1, r)
    where
        (x1, r) = injectParameters x p

instance (InjectParameters (a p), InjectParameters (b p)) => InjectParameters ((a :+: b) p) where
  injectParameters (L1 x) p = (L1 x1, r)
    where
        (x1, r) = injectParameters x p
  injectParameters (R1 x) p = (R1 x1, r)
    where
        (x1, r) = injectParameters x p

instance (InjectParameters (a p), InjectParameters (b p)) => InjectParameters ((a :*: b) p) where
  injectParameters (a :*: b) p = 
    let (a1, p1) = injectParameters a p in
    let (b1, r) = injectParameters b p1 in
    ((a1 :*: b1), r)

