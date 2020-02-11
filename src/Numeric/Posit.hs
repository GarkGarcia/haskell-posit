{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.Posit (Posit32) where

import Data.Int (Int32, Int64)
import Data.Ratio (numerator, denominator)

foreign import ccall "int_to_posit32" intToPosit32 :: Int64 -> Int32
foreign import ccall "posit32_add" p32Add :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_sub" p32Sub :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_mul" p32Mul :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_div" p32Div :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_neg" p32Neg :: Int32 -> Int32
foreign import ccall "posit32_eq" p32Eq :: Int32 -> Int32 -> Bool
foreign import ccall "posit32_le" p32Le :: Int32 -> Int32 -> Bool
foreign import ccall "posit32_lt" p32Lt :: Int32 -> Int32 -> Bool

newtype Posit32 = Posit32 Int32
newtype Posit64 = Posit64 Int64

zero32 :: Posit32
zero32 = Posit32 0

instance Eq Posit32 where
    (Posit32 a) == (Posit32 b) = p32Eq a b

instance Ord Posit32 where
    (Posit32 a) <= (Posit32 b) = p32Le a b
    (Posit32 a) <  (Posit32 b) = p32Lt a b

instance Num Posit32 where
    fromInteger = Posit32 . intToPosit32 . fromInteger 

    (Posit32 a) + (Posit32 b) = Posit32 $ p32Add a b
    (Posit32 a) - (Posit32 b) = Posit32 $ p32Sub a b
    (Posit32 a) * (Posit32 b) = Posit32 $ p32Mul a b
    
    negate (Posit32 a) = Posit32 $ p32Neg a

    signum p@(Posit32 a)
        | a == 0 = zero32 
        | p > zero32 = fromInteger 1 
        | otherwise = fromInteger (-1)

    abs p
        | p < zero32 = negate p
        | otherwise = p

instance Fractional Posit32 where
    fromRational q = Posit32 $ p32Div n d
        where n = intToPosit32 $ fromInteger $ numerator q 
              d = intToPosit32 $ fromInteger $ denominator q 
    
    (Posit32 a) / (Posit32 b) = Posit32 $ p32Div a b

