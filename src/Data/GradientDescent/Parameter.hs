{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.GradientDescent.Parameter where

import Numeric.Natural
import GHC.Generics
import Data.Proxy

newtype Parameter = Parameter { unParameter :: Double }
  deriving (Show, Eq, Num, Fractional)
