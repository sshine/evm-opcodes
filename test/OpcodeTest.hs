{-# LANGUAGE OverloadedStrings #-}

module OpcodeTest where

import Prelude hiding (LT, EQ, GT)

import Data.Foldable (for_)

import           Data.Char (isSpace)
import           Data.DoubleWord (Word256)
import           Data.TinyWord (Word2, Word4)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import EVM.Opcode as Opcode
import EVM.Opcode.Positional as P
import EVM.Opcode.Labelled as L

import OpcodeGenerators

hprop_Jump_and_PUSHfree_opcodes_have_size_1 :: Property
hprop_Jump_and_PUSHfree_opcodes_have_size_1 = property $ do
  opcode <- forAll genOpcode0
  opcodeSize opcode === 1

hprop_opcodeSize_and_opcodeText_for_PUSH_has_size_N_plus_1 :: Property
hprop_opcodeSize_and_opcodeText_for_PUSH_has_size_N_plus_1 = property $ do
  (n, k) <- forAll genWord256'
  opcodeSize (PUSH k) === n + 1

  let got = Text.takeWhile (not . isSpace) (opcodeText (PUSH k))
      exp = "push" <> Text.pack (show n)

  got === exp

hprop_translate_LabelledOpcode :: Property
hprop_translate_LabelledOpcode = property $ do
  labelledOpcodes <- forAll genLabelledOpcodes

  -- Property: Every address jumped to occurs at least once translates correctly.
  positionedOpcodes <- evalEither (L.translate labelledOpcodes)

  -- Property: Translating labels to positions is structure-preserving
  let pairs = zip labelledOpcodes positionedOpcodes
  length labelledOpcodes === length positionedOpcodes

  for_ pairs $ \(lop, pop) ->
    Opcode.concrete lop === Opcode.concrete pop

  -- Property: For every translated, positional jump, the corresponding index is a JUMPDEST.


  -- Property: JUMP/JUMPI near to a border (e.g. 254, 255, 256, 257) works.
  -- Depends on: Opcode generator where size determines size of N in JUMP -> PUSH_n.
