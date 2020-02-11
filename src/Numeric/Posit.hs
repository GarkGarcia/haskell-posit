{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.Posit (Posit) where

import Data.Int (Int32, Int64)
import Data.Ratio (numerator, denominator)
foreign import ccall "int_to_posit" intToPosit :: Int64 -> Int32
foreign import ccall "posit_add" pAdd :: Int32 -> Int32 -> Int32
foreign import ccall "posit_sub" pSub :: Int32 -> Int32 -> Int32
foreign import ccall "posit_mul" pMul :: Int32 -> Int32 -> Int32
foreign import ccall "posit_div" pDiv :: Int32 -> Int32 -> Int32
foreign import ccall "posit_neg" pNeg :: Int32 -> Int32
foreign import ccall "posit_eq" pEq :: Int32 -> Int32 -> Bool
foreign import ccall "posit_le" pLe :: Int32 -> Int32 -> Bool
foreign import ccall "posit_lt" pLt :: Int32 -> Int32 -> Bool

newtype Posit = Posit Int32

zero :: Posit
zero = Posit 0

instance Eq Posit where
    (Posit a) == (Posit b) = pEq a b

instance Ord Posit where
    (Posit a) <= (Posit b) = pLe a b
    (Posit a) <  (Posit b) = pLt a b

instance Num Posit where
    fromInteger = Posit . intToPosit . fromInteger 

    (Posit a) + (Posit b) = Posit $ pAdd a b
    (Posit a) - (Posit b) = Posit $ pSub a b
    (Posit a) * (Posit b) = Posit $ pMul a b
    
    negate (Posit a) = Posit $ pNeg a

    signum p@(Posit a)
        | a == 0 = zero 
        | p > zero = fromInteger 1 
        | otherwise = fromInteger (-1)

    abs p
        | p < zero = negate p
        | otherwise = p

instance Fractional Posit where
    fromRational q = Posit $ pDiv n d
        where n = intToPosit $ fromInteger $ numerator q 
              d = intToPosit $ fromInteger $ denominator q 
    
    (Posit a) / (Posit b) = Posit $ pDiv a b

