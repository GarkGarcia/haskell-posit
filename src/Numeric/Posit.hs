{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.Posit (Posit, Posit8, Posit16, Posit32) where

import Foreign.Storable
import Foreign.Ptr (castPtr)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ratio (numerator, denominator)

foreign import ccall "int_to_posit8" intToPosit8 :: Int64 -> Int8
foreign import ccall "posit8_add" p8Add :: Int8 -> Int8 -> Int8
foreign import ccall "posit8_sub" p8Sub :: Int8 -> Int8 -> Int8
foreign import ccall "posit8_mul" p8Mul :: Int8 -> Int8 -> Int8
foreign import ccall "posit8_div" p8Div :: Int8 -> Int8 -> Int8
foreign import ccall "posit8_neg" p8Neg :: Int8 -> Int8
foreign import ccall "posit8_eq"  p8Eq  :: Int8 -> Int8 -> Bool
foreign import ccall "posit8_le"  p8Le  :: Int8 -> Int8 -> Bool
foreign import ccall "posit8_lt"  p8Lt  :: Int8 -> Int8 -> Bool

foreign import ccall "int_to_posit16" intToPosit16 :: Int64 -> Int16
foreign import ccall "posit16_add" p16Add :: Int16 -> Int16 -> Int16
foreign import ccall "posit16_sub" p16Sub :: Int16 -> Int16 -> Int16
foreign import ccall "posit16_mul" p16Mul :: Int16 -> Int16 -> Int16
foreign import ccall "posit16_div" p16Div :: Int16 -> Int16 -> Int16
foreign import ccall "posit16_neg" p16Neg :: Int16 -> Int16
foreign import ccall "posit16_eq"  p16Eq  :: Int16 -> Int16 -> Bool
foreign import ccall "posit16_le"  p16Le  :: Int16 -> Int16 -> Bool
foreign import ccall "posit16_lt"  p16Lt  :: Int16 -> Int16 -> Bool

foreign import ccall "int_to_posit32" intToPosit32 :: Int64 -> Int32
foreign import ccall "posit32_add" p32Add :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_sub" p32Sub :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_mul" p32Mul :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_div" p32Div :: Int32 -> Int32 -> Int32
foreign import ccall "posit32_neg" p32Neg :: Int32 -> Int32
foreign import ccall "posit32_eq"  p32Eq  :: Int32 -> Int32 -> Bool
foreign import ccall "posit32_le"  p32Le  :: Int32 -> Int32 -> Bool
foreign import ccall "posit32_lt"  p32Lt  :: Int32 -> Int32 -> Bool

-- | 8 bit posit numbers.
newtype Posit8  = Posit8  Int8

-- | 16 bit posit numbers.
newtype Posit16 = Posit16 Int16

-- | 32 bit posit numbers.
newtype Posit32 = Posit32 Int32

type Posit = Posit32

zero8 :: Posit8
zero8 = Posit8 0

instance Eq Posit8 where
    (Posit8 a) == (Posit8 b) = p8Eq a b

instance Ord Posit8 where
    (Posit8 a) <= (Posit8 b) = p8Le a b
    (Posit8 a) <  (Posit8 b) = p8Lt a b

instance Num Posit8 where
    fromInteger = Posit8 . intToPosit8 . fromInteger 

    (Posit8 a) + (Posit8 b) = Posit8 $ p8Add a b
    (Posit8 a) - (Posit8 b) = Posit8 $ p8Sub a b
    (Posit8 a) * (Posit8 b) = Posit8 $ p8Mul a b
    
    negate (Posit8 a) = Posit8 $ p8Neg a

    signum p
        | p == zero8 = zero8 
        | p > zero8  = fromInteger 1 
        | otherwise  = fromInteger (-1)

    abs p
        | p < zero8 = negate p
        | otherwise = p

instance Fractional Posit8 where
    fromRational q = Posit8 $ p8Div n d
        where n = intToPosit8 $ fromInteger $ numerator q 
              d = intToPosit8 $ fromInteger $ denominator q 
    
    (Posit8 a) / (Posit8 b) = Posit8 $ p8Div a b

instance Storable Posit8 where
    sizeOf _ = 1
    alignment = sizeOf

    peek p = Posit8 <$> peek (castPtr p)
    poke p (Posit8 a) = poke (castPtr p) a

zero16 :: Posit16
zero16 = Posit16 0

instance Eq Posit16 where
    (Posit16 a) == (Posit16 b) = p16Eq a b

instance Ord Posit16 where
    (Posit16 a) <= (Posit16 b) = p16Le a b
    (Posit16 a) <  (Posit16 b) = p16Lt a b

instance Num Posit16 where
    fromInteger = Posit16 . intToPosit16 . fromInteger 

    (Posit16 a) + (Posit16 b) = Posit16 $ p16Add a b
    (Posit16 a) - (Posit16 b) = Posit16 $ p16Sub a b
    (Posit16 a) * (Posit16 b) = Posit16 $ p16Mul a b
    
    negate (Posit16 a) = Posit16 $ p16Neg a

    signum p
        | p == zero16 = zero16 
        | p > zero16  = fromInteger 1 
        | otherwise   = fromInteger (-1)

    abs p
        | p < zero16 = negate p
        | otherwise  = p

instance Fractional Posit16 where
    fromRational q = Posit16 $ p16Div n d
        where n = intToPosit16 $ fromInteger $ numerator q 
              d = intToPosit16 $ fromInteger $ denominator q 
    
    (Posit16 a) / (Posit16 b) = Posit16 $ p16Div a b

instance Storable Posit16 where
    sizeOf _ = 2
    alignment = sizeOf

    peek p = Posit16 <$> peek (castPtr p)
    poke p (Posit16 a) = poke (castPtr p) a

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

    signum p
        | p == zero32 = zero32 
        | p > zero32  = fromInteger 1 
        | otherwise   = fromInteger (-1)

    abs p
        | p < zero32 = negate p
        | otherwise = p

instance Fractional Posit32 where
    fromRational q = Posit32 $ p32Div n d
        where n = intToPosit32 $ fromInteger $ numerator q 
              d = intToPosit32 $ fromInteger $ denominator q 
    
    (Posit32 a) / (Posit32 b) = Posit32 $ p32Div a b

instance Storable Posit32 where
    sizeOf _ = 4
    alignment = sizeOf

    peek p = Posit32 <$> peek (castPtr p)
    poke p (Posit32 a) = poke (castPtr p) a

