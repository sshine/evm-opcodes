{-# LANGUAGE OverloadedStrings #-}

module OpcodeTest where

import Prelude hiding (LT, EQ, GT)

import           Data.Char (isSpace)
import           Data.ByteString (index)
import           Data.DoubleWord (Word256)
import           Data.Foldable (for_)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import EVM.Opcode as Opcode
import EVM.Opcode.Positional as P
import EVM.Opcode.Labelled as L

import OpcodeGenerators

hprop_opcodeSize_1 :: Property
hprop_opcodeSize_1 = property $ do
  opcode <- forAll genOpcode'1
  opcodeSize opcode === 1

-- | @n@ is 0-31 (so + 1), and PUSH itself takes 1 byte (so + 1).
hprop_opcodeSize_PUSH :: Property
hprop_opcodeSize_PUSH = property $ do
  (n, k) <- forAll genWord256'
  opcodeSize (PUSH k) === n + 1 + 1

hprop_opcodeText_for_PUSH_matches :: Property
hprop_opcodeText_for_PUSH_matches = property $ do
  (n, k) <- forAll genWord256'
  let text = opcodeText (PUSH k)
      exp = "push" <> Text.pack (show (n + 1))
      got = Text.takeWhile (not . isSpace) text

  got === exp

hprop_opcodeSpec_unique :: Property
hprop_opcodeSpec_unique = property $ do
  opcode1 <- forAll genOpcode
  opcode2 <- forAll genOpcode

  if opcode1 == opcode2
    then opcodeSpec opcode1 === opcodeSpec opcode2
    else opcodeSpec opcode1 /== opcodeSpec opcode2

hprop_translate_LabelledOpcode :: Property
hprop_translate_LabelledOpcode = withTests 10000 $ property $ do
  labelledOpcodes <- forAll genLabelledOpcodes

  -- Property: Labelled opcodes, for which valid jumpdests occur, translate.
  positionalOpcodes <- evalEither (L.translate labelledOpcodes)

  -- Property: Translating labels to positions is structure-preserving.
  let pairs = zip labelledOpcodes positionalOpcodes
  fmap Opcode.concrete labelledOpcodes === fmap Opcode.concrete positionalOpcodes

  -- Property: For every positional jump the corresponding index in the translated
  -- bytecode is a JUMPDEST.
  let positions = mapMaybe jumpAnnot positionalOpcodes
  let opcodes = P.translate positionalOpcodes
  let bytecode = Opcode.pack opcodes

  -- FIXME: bytestring-0.11.0.0 has `indexMaybe` / `!?`.
  -- bytecode !? fromIntegral pos === Just jumpdest
  for_ positions $ \pos ->
    [ bytecode `index` fromIntegral pos ] === toBytes jumpdest

  -- Property: JUMP/JUMPI near to a border (e.g. 254, 255, 256, 257) works.
  -- Depends on: Opcode generator where size determines size of N in JUMP -> PUSH_n.

-- Property: read/show identity for Opcode, PositionalOpcode, LabelledOpcode
