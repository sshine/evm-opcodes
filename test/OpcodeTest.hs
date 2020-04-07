{-# LANGUAGE OverloadedStrings #-}

module OpcodeTest where

import Prelude hiding (LT, EQ, GT)

import Control.Monad

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.TinyWord (Word2, Word4)
import Data.LargeWord (Word256)
import Network.Ethereum.Evm.Opcode
import Network.Ethereum.Evm.PositionedOpcode
import Network.Ethereum.Evm.LabelledOpcode

import OpcodeGenerators

hprop_Jump_and_PUSHfree_opcodes_have_size_1 :: Property
hprop_Jump_and_PUSHfree_opcodes_have_size_1 = property $ do
  opcode <- forAll (Gen.choice [ genOpcode0, genOpcode1 ])
  opcodeSize opcode === 1

hprop_opcodeSize_and_opcodeText_for_PUSH_has_size_N_plus_1 :: Property
hprop_opcodeSize_and_opcodeText_for_PUSH_has_size_N_plus_1 = property $ do
  (n, k) <- forAll genWord256'
  opcodeSize (PUSH k) === n + 1

  let gotPretty = Text.takeWhile (not . isSpace) (opcodeText (PUSH k))
  gotPretty === "push" <> Text.pack (show n)

-- Property: PositionalOpcode JUMP translates to PUSH and JUMP.
--           (Every JUMP is preceded by a PUSH.)

-- Property:

-- 0: Gen Label


-- 1: Gen [LabelledOpcode] for straight-line code

