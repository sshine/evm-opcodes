
module OpcodeGenerators where

import Prelude hiding (LT, EQ, GT)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.TinyWord (Word2, Word4)
import Data.LargeWord (Word256)
import Network.Ethereum.Evm.Opcode
import Network.Ethereum.Evm.PositionedOpcode
--import Network.Ethereum.Evm.LabelledOpcode

-- | Generate a jump-free `Opcode` with zero arity.
genOpcode0 :: Gen (AbstractOpcode a)
genOpcode0 = Gen.element
  [ STOP, ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, ADDMOD, MULMOD, EXP, SIGNEXTEND
  , LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, NOT, BYTE
  , SHA3
  , ADDRESS, BALANCE, ORIGIN, CALLER, CALLVALUE, CALLDATALOAD, CALLDATASIZE, CALLDATACOPY, CODESIZE, CODECOPY, GASPRICE, EXTCODESIZE, EXTCODECOPY
  , BLOCKHASH, COINBASE, TIMESTAMP, NUMBER, DIFFICULTY, GASLIMIT
  , POP, MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE {- , JUMP, JUMPI -}, PC, MSIZE, GAS {- , JUMPDEST -}
    {- , PUSH, DUP, SWAP, LOG -}
  , CREATE, CALL, CALLCODE, RETURN, DELEGATECALL, SUICIDE
  ]

-- | Generate a jump-free `Opcode` with arity 1, size 1.
genOpcode1 :: Gen (AbstractOpcode a)
genOpcode1 = Gen.choice
  [ DUP <$> genWordN
  , SWAP <$> genWordN
  , LOG <$> genWordN
  ]

genPushOpcode :: Gen (AbstractOpcode a)
genPushOpcode = PUSH <$> genWord256

genPushOpcode' :: Gen (Int, AbstractOpcode a)
genPushOpcode' = do
  (n, k) <- genWord256'
  pure (1 + n, PUSH k)

genPosJumpOpcode :: Gen PositionedOpcode
genPosJumpOpcode = Gen.choice
  [ JUMP <$> genWord256
  , JUMPI <$> genWord256
  , JUMPDEST <$> genWord256
  ]

-- Generate `PositionalOpcode` with arbitrary jumps.
genPosOpcode :: Gen PositionedOpcode
genPosOpcode = Gen.frequency
  [ (1, genPushOpcode)
  , (1, genPosJumpOpcode)
  , (3, genOpcode0)
  ]

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
  bytes <- Gen.integral (Range.linear 0 31)
  let lo = 2 ^ (8 * bytes)
      hi = 2 ^ (8 * (bytes + 1))
  k <- Gen.integral_ (Range.constant lo hi)
  pure (bytes, k - 1)
