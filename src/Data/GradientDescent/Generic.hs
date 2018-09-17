{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.GradientDescent.Generic where

import Numeric.Natural
import GHC.Generics
import Data.Proxy
import Data.GradientDescent.Parameter

epsilon :: Double
epsilon = 0.0000001

genericStep :: (ExtractParameters a, InjectParameters a) => (a -> Double) -> Double -> a -> a
genericStep f scale a = fst $ injectParameters a modifiedParameters
    where
        parameters :: [Double]
        parameters = extractParameters a
        indexes :: [Int]
        indexes = [0..length parameters]
        base :: Double
        base = evaluateParameters parameters
        evaluateParameters :: [Double] -> Double
        evaluateParameters p = f r
          where
            (r, []) = injectParameters a p
        microIncrement :: Double -> Double
        microIncrement x = abs $ epsilon * x
        deltas :: [Double]
        deltas = calcDelta <$> indexes
        modifiedDeltas :: [Double]
        modifiedDeltas = (* scale) <$> deltas
        modifiedParameters :: [Double]
        modifiedParameters = zipWith (-) parameters modifiedDeltas
        calcDelta :: Int -> Double
        calcDelta p = delta
            where
                parameter :: Double
                parameter = parameters !! p
                increment :: Double
                increment = microIncrement parameter
                modified :: [Double]
                modified = replace p parameters (parameter + increment)
                replace :: Int -> [a] -> a -> [a]
                replace 0 (h:t) a = a:t
                replace n (h:t) a = h : replace (n-1) t a
                evaluated :: Double
                evaluated = evaluateParameters modified
                delta = (evaluated - base) / increment


class ExtractParameters a where
  extractParameters :: a -> [Double]
  default extractParameters :: (Generic a, GExtractParameters (Rep a)) => a -> [Double]
  extractParameters x = gExtractParameters $ from x

class GExtractParameters f where
  gExtractParameters :: f p -> [Double]

instance GExtractParameters V1 where
  gExtractParameters _ = []

instance GExtractParameters U1 where
  gExtractParameters _ = []

instance (ExtractParameters a) => GExtractParameters (K1 i a) where
  gExtractParameters (K1 x) = extractParameters x

instance ExtractParameters Parameter where
  extractParameters (Parameter x) = [x]

instance ExtractParameters Double where
  extractParameters _ = []

instance GExtractParameters f => GExtractParameters (M1 i c f) where
  gExtractParameters (M1 x) = gExtractParameters x

instance (GExtractParameters a, GExtractParameters b) => GExtractParameters (a :+: b) where
  gExtractParameters (L1 x) = gExtractParameters x
  gExtractParameters (R1 x) = gExtractParameters x

instance (GExtractParameters a, GExtractParameters b) => GExtractParameters (a :*: b) where
  gExtractParameters (a :*: b) = gExtractParameters a ++ gExtractParameters b


class InjectParameters a where
  injectParameters :: a -> [Double] -> (a, [Double])
  default injectParameters :: (Generic a, GInjectParameters (Rep a)) => a -> [Double] -> (a, [Double])
  injectParameters x l = (to x1, r)
    where
      (x1, r) = gInjectParameters (from x) l

class GInjectParameters f where
  gInjectParameters :: f a -> [Double] -> (f a, [Double])

instance GInjectParameters V1 where
  gInjectParameters x p = (x, p)

instance GInjectParameters U1 where
  gInjectParameters x p = (x, p)

instance InjectParameters Parameter where
  injectParameters (Parameter _) (h:t) = (Parameter h, t)

instance InjectParameters Double where
  injectParameters x p = (x, p)

instance (InjectParameters a) => GInjectParameters (K1 i a) where
    gInjectParameters (K1 a) l = (K1 x1, r)
      where
          (x1, r) = injectParameters a l

instance GInjectParameters f => GInjectParameters (M1 i c f) where
  gInjectParameters (M1 x) p = (M1 x1, r)
    where
        (x1, r) = gInjectParameters x p

instance (GInjectParameters a, GInjectParameters b) => GInjectParameters (a :+: b) where
  gInjectParameters (L1 x) p = (L1 x1, r)
    where
        (x1, r) = gInjectParameters x p
  gInjectParameters (R1 x) p = (R1 x1, r)
    where
        (x1, r) = gInjectParameters x p

instance (GInjectParameters a, GInjectParameters b) => GInjectParameters (a :*: b) where
  gInjectParameters (a :*: b) p = 
    let (a1, p1) = gInjectParameters a p in
    let (b1, r) = gInjectParameters b p1 in
    ((a1 :*: b1), r)
