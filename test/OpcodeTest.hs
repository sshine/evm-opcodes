{-# LANGUAGE OverloadedStrings #-}

module OpcodeTest where

import Prelude hiding (LT, EQ, GT)

import Control.Monad

import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.TinyWord (Word2, Word4)
import Data.LargeWord (Word256)
import Network.Ethereum.Evm.Opcode
--import Network.Ethereum.Evm.PositionedOpcode
--import Network.Ethereum.Evm.LabelledOpcode

-- Step 0: Gen Opcode (0 arity)
opcode0 :: Gen (AbstractOpcode a)
opcode0 = Gen.element
  [ STOP, ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, ADDMOD, MULMOD, EXP, SIGNEXTEND
  , LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, NOT, BYTE
  , SHA3
  , ADDRESS, BALANCE, ORIGIN, CALLER, CALLVALUE, CALLDATALOAD, CALLDATASIZE, CALLDATACOPY, CODESIZE, CODECOPY, GASPRICE, EXTCODESIZE, EXTCODECOPY
  , BLOCKHASH, COINBASE, TIMESTAMP, NUMBER, DIFFICULTY, GASLIMIT
  , POP, MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE {- , JUMP, JUMPI -}, PC, MSIZE, GAS {- , JUMPDEST -}
    {- , PUSH, DUP, SWAP, LOG -}
  , CREATE, CALL, CALLCODE, RETURN, DELEGATECALL, SUICIDE
  ]

-- Step 1: Gen Opcode (1 arity: Word2, Word4, Word256)
opcode1 :: Gen (AbstractOpcode a)
opcode1 = Gen.choice
  [ PUSH <$> genWord256
  , DUP <$> genWordN
  , SWAP <$> genWordN
  , LOG <$> genWordN
  ]

genWordN :: (MonadGen m, Integral w, Bounded w) => m w
genWordN = Gen.integral Range.constantBounded

-- | Generate an arbitrary 256-bit constant with the number of bytes linearly
-- dependent on the generator's size and with the actual value being uniformly
-- distributed in the interval of available bytes, independent of the size:
--
-- 0 giving 0-255, 1 being 256-65535, and so on.
genWord256 :: Gen Word256
genWord256 = do
  bits <- Gen.integral (Range.linear 0 31)
  let lo = 2 ^ (8 * bits)
      hi = 2 ^ (8 * (bits + 1))
  pred <$> Gen.integral_ (Range.constant lo hi)

-- Step 2: Gen Opcode (1 arity: JUMP, JUMPI, JUMPDEST)


-- Property: A `PositionedOpcode` jump always translates to one more jump.
-- Property: The `jumpSize` is 1 + N + 1 (PUSH + constant + JUMP).
