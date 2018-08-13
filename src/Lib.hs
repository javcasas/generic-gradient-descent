{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
    ( someFunc
    ) where

import GHC.Generics
someFunc :: IO ()
someFunc = do
    sweep (Example (Parameter 0) 1 1 1)
    multiStepDescent 1 100 (Example (Parameter 48) 1 1 1)

data Parameter a = Parameter a deriving (Show, Eq)
unParameter (Parameter x) = x

data Example = Example {
    ex :: Parameter Double,
    ea :: Double,
    eb :: Double,
    ec :: Double
  } deriving (Show, Generic)

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
