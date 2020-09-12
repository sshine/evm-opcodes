
module OpcodeGenerators where

import Prelude hiding (LT, EQ, GT)

import           Data.TinyWord (Word2, Word4)
import           Data.DoubleWord (Word256)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Containers.ListUtils (nubOrd)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           EVM.Opcode
import           EVM.Opcode.Positional
import           EVM.Opcode.Labelled
import           EVM.Opcode.Traversal

-- | Generate a jump-free `Opcode` with zero arity.
genOpcode0 :: Gen (Opcode' a)
genOpcode0 = Gen.frequency
  [ (length opcode0, Gen.element opcode0)
  , (length opcode1, Gen.choice opcode1)
  ]
  where
    opcode0 =
      [ STOP, ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, ADDMOD, MULMOD, EXP, SIGNEXTEND
      , LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, NOT, BYTE, SHL, SHR, SAR
      , SHA3

      , ADDRESS, BALANCE, ORIGIN
      , CALLER, CALLVALUE, CALLDATALOAD, CALLDATASIZE, CALLDATACOPY
      , CODESIZE, CODECOPY
      , GASPRICE, EXTCODESIZE, EXTCODECOPY, RETURNDATASIZE, RETURNDATACOPY, EXTCODEHASH

      , BLOCKHASH, COINBASE, TIMESTAMP, NUMBER, DIFFICULTY, GASLIMIT, CHAINID, SELFBALANCE

      , POP, MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE {- , JUMP, JUMPI -}, PC, MSIZE, GAS {- , JUMPDEST -}
        {- , PUSH, DUP, SWAP, LOG -}
      , CREATE, CALL, CALLCODE, RETURN, DELEGATECALL, CREATE2, STATICCALL, REVERT, INVALID, SELFDESTRUCT
      ]

    opcode1 =
      [ DUP <$> genWordN
      , SWAP <$> genWordN
      , LOG <$> genWordN
      ]

genPushOpcode :: Gen (Opcode' a)
genPushOpcode = PUSH <$> genWord256

genPositionalJump :: Gen PositionalOpcode
genPositionalJump = Gen.choice
  [ JUMP <$> genWord256
  , JUMPI <$> genWord256
  , JUMPDEST <$> genWord256
  ]

-- | Generate a list of `LabelledOpcode` where all JUMP/JUMPI have a JUMPDEST.
genLabelledOpcodes :: Gen [LabelledOpcode]
genLabelledOpcodes = do
  opcodes <- Gen.list (Range.linear 0 40) genLabelledOpcode
  let jumpdests = fmap JUMPDEST . nubOrd . foldMap extractLabel $ opcodes
  Gen.shuffle (opcodes <> jumpdests)
  where
    extractLabel :: LabelledOpcode -> [Label]
    extractLabel (JUMP label) = [label]
    extractLabel (JUMPI label) = [label]
    extractLabel _ = []

-- | Generate a `LabelledOpcode` that isn't JUMPDEST.
genLabelledOpcode :: Gen LabelledOpcode
genLabelledOpcode = Gen.frequency
  [ (1, genPushOpcode)
  , (1, genLabelledJump)
  , (3, genOpcode0)
  ]

genLabelledJump :: Gen LabelledOpcode
genLabelledJump = Gen.choice [ JUMP <$> genLabel, JUMPI <$> genLabel ]

genLabel :: Gen Text
genLabel = Gen.text (Range.singleton 3) (Gen.element "xyz")

-- | Generate a Word of size N.
genWordN :: (MonadGen m, Integral w, Bounded w) => m w
genWordN = Gen.integral Range.constantBounded

-- | Generate a `Word256` with the number of bytes linearly
-- dependent on the generator's size and with the actual value being uniformly
-- distributed in the interval of available bytes, independent of the size:
--
-- 0 giving 0-255, 1 being 256-65535, and so on.
genWord256 :: Gen Word256
genWord256 = snd <$> genWord256'

-- | Generate a byte size and a `Word256` of that byte size.
genWord256' :: Gen (Int, Word256)
genWord256' = do
  bytes <- Gen.integral (Range.linear 1 32)
  let lo = 2 ^ (8 * (bytes - 1))
      hi = 2 ^ (8 * bytes)
  k <- pred <$> Gen.integral_ (Range.constant lo hi)
  pure (bytes, k)
