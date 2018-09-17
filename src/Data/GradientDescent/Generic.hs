{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.GradientDescent.Generic where

import Numeric.Natural
import GHC.Generics
import Data.Proxy

epsilon :: Double
epsilon = 0.0000001

genericStep (ExtractParameters a, InjectParameters a) => (a -> Double) -> Double -> a -> a
genericStep f l a = injectParameters a modifiedParameters
    where
        parameters :: [Double]
        parameters = extractParameters a
        indexes :: [Int]
        indexes = [0..length parameters]
        base :: Double
        base = evaluateParameters parameters
        evaluateParameters :: [Double] -> Double
        evaluateParameters p = f $ injectParameters a p
        microIncrement :: Double -> Double
        microIncrement x = abs $ epsilon * x
        deltas :: [Double]
        deltas = calcDelta <$> indexes
        modifiedDeltas :: [Double]
        modifiedDeltas = (* l) <$> deltas
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
  default extractParameters :: (Generic a, ExtractParameters1 (Rep a)) => a -> [Double]
  extractParameters x = extractParameters1 $ from x

class ExtractParameters1 f where
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
