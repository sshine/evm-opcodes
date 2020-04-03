-- |
-- Module: Network.Ethereum.Evm.PositionedOpcode
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exposes the `PositionedOpcode` type for expressing Ethereum
-- VM opcodes where jumps and jumpdests are annotated with the byte position
-- of the translated opcode.
--
-- This representation is useful for when generating code that refers to the
-- size of itself or other chunks of code.  E.g. the CODECOPY segment of an
-- Ethereum contract must refer to the size of the code being copied, and
-- determining the size of a jump is trivial when it's annotated with the
-- destination address.

module Network.Ethereum.Evm.PositionedOpcode where

import Data.LargeWord (Word256)

import Network.Ethereum.Evm.Opcode (Opcode, AbstractOpcode(..), jump, jumpi, jumpdest, concrete, opcodeSize)

-- | The position of an Opcode.
type Position = Word256

-- | A `PositionedOpcode` has byte positions annotated at `JUMP`, `JUMPI`
-- and `JUMPDEST`; on `JUMP` and `JUMPI` the positions denote where they
-- jump to, and on `JUMPDEST` they denote the position of the opcode itself.
type PositionedOpcode = AbstractOpcode Position

translate :: [PositionedOpcode] -> [Opcode]
translate = concatMap inline
  where
    inline :: PositionedOpcode -> [Opcode]
    inline (JUMP addr) = [ PUSH addr, jump ]
    inline (JUMPI addr) = [ PUSH addr, jumpi ]
    inline (JUMPDEST _addr) = [ jumpdest ]
    inline opcode = [ concrete opcode ]

-- | The size of a jump to some absolute position.
jumpSize :: Num i => Position -> i
jumpSize pos = opcodeSize (PUSH $ fromIntegral pos) + opcodeSize jump
