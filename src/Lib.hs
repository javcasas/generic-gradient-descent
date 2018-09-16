{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Numeric.Natural
import GHC.Generics
import Data.Proxy
someFunc :: IO ()
someFunc = pure ()
    --sweep (Example (Parameter 0) 1 1 1)
    --multiStepDescent 1 100 (Example (Parameter 48) 1 1 1)
    --print $ extractParameters1 $ from $ (Example (Parameter 48) 1 1 1)

newtype Parameter a = Parameter { unParameter :: a } deriving (Show, Eq, Num)

{-
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
-}
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
  default extractParameters :: (Generic a, ExtractParameters1 (Rep a)) => a -> [Double]
  extractParameters x = extractParameters1 $ from x

class ExtractParameters1 f where
  -- | Return number of constuctor fields for a value.
  extractParameters1 :: f p -> [Double]

instance ExtractParameters1 V1 where
  extractParameters1 _ = []

instance ExtractParameters1 U1 where
  extractParameters1 _ = []

instance ExtractParameters1 (K1 i (Parameter Double)) where
  extractParameters1 (K1 (Parameter x)) = [x]

instance ExtractParameters1 (K1 i Double) where
  extractParameters1 _ = []

instance ExtractParameters1 f => ExtractParameters1 (M1 i c f) where
  extractParameters1 (M1 x) = extractParameters1 x

instance (ExtractParameters1 a, ExtractParameters1 b) => ExtractParameters1 (a :+: b) where
  extractParameters1 (L1 x) = extractParameters1 x
  extractParameters1 (R1 x) = extractParameters1 x

instance (ExtractParameters1 a, ExtractParameters1 b) => ExtractParameters1 (a :*: b) where
  extractParameters1 (a :*: b) = extractParameters1 a ++ extractParameters1 b


class InjectParameters a where
  injectParameters :: a -> [Double] -> a
  default injectParameters :: (Generic a, InjectParameters1 (Rep a)) => a -> [Double] -> a
  injectParameters x l = to r
    where
        (r, []) = injectParameters1 (from x) l

class InjectParameters1 a where
  injectParameters1 :: a b -> [Double] -> (a b, [Double])

instance InjectParameters1 V1 where
  injectParameters1 x p = (x, p)

instance InjectParameters1 U1 where
  injectParameters1 x p = (x, p)

instance InjectParameters1 (K1 i (Parameter Double)) where
  injectParameters1 (K1 (Parameter x)) (h:t) = (K1 (Parameter h), t)

instance InjectParameters1 (K1 i Double) where
  injectParameters1 x p = (x, p)

instance InjectParameters1 f => InjectParameters1 (M1 i c f) where
  injectParameters1 (M1 x) p = (M1 x1, r)
    where
        (x1, r) = injectParameters1 x p

instance (InjectParameters1 a, InjectParameters1 b) => InjectParameters1 (a :+: b) where
  injectParameters1 (L1 x) p = (L1 x1, r)
    where
        (x1, r) = injectParameters1 x p
  injectParameters1 (R1 x) p = (R1 x1, r)
    where
        (x1, r) = injectParameters1 x p

instance (InjectParameters1 a, InjectParameters1 b) => InjectParameters1 (a :*: b) where
  injectParameters1 (a :*: b) p = 
    let (a1, p1) = injectParameters1 a p in
    let (b1, r) = injectParameters1 b p1 in
    ((a1 :*: b1), r)

