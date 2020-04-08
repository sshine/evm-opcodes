{-# LANGUAGE OverloadedStrings #-}

module OpcodeTest where

import Prelude hiding (LT, EQ, GT)

import Data.Foldable (for_)

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.TinyWord (Word2, Word4)
import Data.LargeWord (Word256)
import Network.Ethereum.Evm.Opcode as Opcode
import Network.Ethereum.Evm.PositionedOpcode as P
import Network.Ethereum.Evm.LabelledOpcode as L

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
  positionedOpcodes <- evalEither (L.translate labelledOpcodes)

  -- Property: The translated code has JUMP, JUMPI and JUMPDEST in the same places.
  for_ (zip labelledOpcodes positionedOpcodes) $ \(lop, pop) ->
    Opcode.concrete lop === Opcode.concrete pop

  -- Property: Code where every address being jumped to occurs at least once translates correctly.
  -- Property: For every translated, positional jump, the corresponding index is a JUMPDEST.
