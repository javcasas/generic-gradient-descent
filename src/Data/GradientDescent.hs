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
module Data.GradientDescent
    ( module Data.GradientDescent.Parameter
    , module Data.GradientDescent.Generic
    , GradientDescent(..)
    , descent
    ) where

import Numeric.Natural
import GHC.Generics
import Data.Proxy
import Data.GradientDescent.Parameter
import Data.GradientDescent.Generic

class GradientDescent a where
    f :: a -> Double

    step :: Double -> a -> a
    default step :: (ExtractParameters a, InjectParameters a) => Double -> a -> a
    step = genericStep f

descent :: (GradientDescent a) => Double -> a -> [a]
descent increment initial = 
    let d = step increment initial in
    d : descent (increment * 0.95) d
