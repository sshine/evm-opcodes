
-- |
-- Module: Data.TinyWord
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exports two types, `Word2` and `Word4`, as wrappers around `Word8`.

module Data.TinyWord where

import Data.Bits
import Data.Word
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen, toEnumError, succError, predError)

newtype Word2 = Word2 { unWord2 :: Word8 } deriving (Eq, Ord)
newtype Word4 = Word4 { unWord4 :: Word8 } deriving (Eq, Ord)

word2 :: Word8 -> Word2
word2 = Word2 . (.&. 0x03)

word4 :: Word8 -> Word4
word4 = Word4 . (.&. 0x0f)

instance Show Word2 where
  show = show . unWord2

instance Show Word4 where
  show = show . unWord4

instance Bounded Word2 where
  minBound = Word2 0x00
  maxBound = Word2 0x03

instance Bounded Word4 where
  minBound = Word4 0x00
  maxBound = Word4 0x0f

instance Enum Word2 where
  succ (Word2 x) = if x < 0x0f then Word2 (succ x) else succError "Word2"
  pred (Word2 x) = if x > 0x00 then Word2 (pred x) else predError "Word2"
  toEnum i | 0x00 <= i && i <= 0x0f = Word2 (toEnum i)
  toEnum i = toEnumError "Word2" i (minBound :: Word2, maxBound :: Word2)
  fromEnum = fromEnum . unWord2
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo (Word2 x) (Word2 y) = map Word2 (enumFromTo x y)
  enumFromThenTo (Word2 x) (Word2 y) (Word2 z) = map Word2 (enumFromThenTo x y z)

instance Enum Word4 where
  succ (Word4 x) = if x < 0x0f then Word4 (succ x) else succError "Word4"
  pred (Word4 x) = if x > 0x00 then Word4 (pred x) else predError "Word4"
  toEnum i | 0x00 <= i && i <= 0x0f = Word4 (toEnum i)
  toEnum i = toEnumError "Word4" i (minBound :: Word4, maxBound :: Word4)
  fromEnum = fromEnum . unWord4
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo (Word4 x) (Word4 y) = map Word4 (enumFromTo x y)
  enumFromThenTo (Word4 x) (Word4 y) (Word4 z) = map Word4 (enumFromThenTo x y z)

instance Num Word2 where
  Word2 x + Word2 y = Word2 $ x + y
  Word2 x * Word2 y = Word2 $ x * y
  Word2 x - Word2 y = Word2 $ x - y
  negate (Word2 x) = Word2 (negate x)
  abs = id
  signum (Word2 x) = Word2 (if x == 0 then 0 else 1)
  fromInteger = word2 . fromInteger

instance Num Word4 where
  Word4 x + Word4 y = Word4 $ x + y
  Word4 x * Word4 y = Word4 $ x * y
  Word4 x - Word4 y = Word4 $ x - y
  negate (Word4 x) = Word4 (negate x)
  abs = id
  signum (Word4 x) = Word4 (if x == 0 then 0 else 1)
  fromInteger = word4 . fromInteger

instance Real Word2 where
  toRational = toRational . unWord2

instance Real Word4 where
  toRational = toRational . unWord4

instance Integral Word2 where
  quotRem (Word2 x) (Word2 y) = (Word2 q, Word2 r) where (q, r) = quotRem x y
  toInteger = toInteger . unWord2

instance Integral Word4 where
  quotRem (Word4 x) (Word4 y) = (Word4 q, Word4 r) where (q, r) = quotRem x y
  toInteger = toInteger . unWord4
