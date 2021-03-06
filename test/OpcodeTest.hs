{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OpcodeTest where

import Prelude hiding (LT, EQ, GT)

import           Data.Char (isSpace)
import qualified Data.ByteString as BS
import           Data.DoubleWord (Word256)
import           Data.Foldable (for_)
import           Data.List (permutations)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty.Hspec
import           Test.Hspec

import EVM.Opcode as Opcode
import EVM.Opcode.Positional as P
import EVM.Opcode.Labelled as L

import OpcodeGenerators

-- Property: Jump-free non-PUSH opcodes have size 1.
hprop_opcodeSize_1 :: Property
hprop_opcodeSize_1 = property $ do
  opcode <- forAll genOpcode'1
  opcodeSize opcode === 1

-- Property: When n is 0-31, PUSH opcodes have size n + 2.
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

hprop_opcodeName_unique :: Property
hprop_opcodeName_unique = property $ do
  opcode1 <- forAll genOpcode
  opcode2 <- forAll genOpcode
  if opcode1 == opcode2
    then opcodeName (opcodeSpec opcode1) === opcodeName (opcodeSpec opcode2)
    else opcodeName (opcodeSpec opcode1) /== opcodeName (opcodeSpec opcode2)

hprop_pack_readOp_inverses :: Property
hprop_pack_readOp_inverses = property $ do
  opcode1 <- forAll genOpcode
  let bytecode = pack [opcode1]

  -- Property: Opcodes are packed to /non-empty/ ByteStrings.
  (c, cs) <- evalMaybe (BS.uncons bytecode)

  -- Property: 'pack' and 'readOp' are inverses.
  opcode2 <- evalMaybe (readOp c cs)
  opcode1 === opcode2

hprop_translate_LabelledOpcode :: Property
hprop_translate_LabelledOpcode = withTests 10000 $ property $ do
  labelledOpcodes <- forAll genLabelledOpcodes

  -- Property: Labelled opcodes, for which valid jumpdests occur, translate.
  positionalOpcodes <- evalEither (L.translate labelledOpcodes)

  -- Property: Translating labels to positions is structure-preserving.
  let pairs = zip labelledOpcodes positionalOpcodes
  fmap Opcode.concrete labelledOpcodes === fmap Opcode.concrete positionalOpcodes

  -- Property: For every positional jump the corresponding index in the translated
  -- bytecode is a JUMPDEST. FIXME: bytestring-0.11.0.0 has `indexMaybe` / `!?`.
  let positions = mapMaybe jumpAnnot positionalOpcodes
  let opcodes = P.translate positionalOpcodes
  let bytecode = Opcode.pack opcodes
  for_ (fromIntegral <$> positions) $ \pos ->
    [ bytecode `BS.index` pos ] === toBytes jumpdest

-- Negative tests to assert that broken labels don't cause infinite recursion

spec_EVM_Opcode_Labelled :: Spec
spec_EVM_Opcode_Labelled = do
  describe "translate" $ do
    it "handles empty lists" $
      L.translate [] `shouldBe` Right []

    it "handles empty labels" $
      L.translate [JUMP "", JUMPDEST ""] `shouldBe` Right [JUMP 3, JUMPDEST 3]

    it "handles jumpdests with no pointers to it" $
      L.translate [JUMPDEST "foo"] `shouldBe` Right [JUMPDEST 0]

    it "fails on jumps without destinations" $ do
      L.translate [JUMP "off"] `shouldMissErr` ["off"]
      L.translate [JUMPI "off"] `shouldMissErr` ["off"]
      L.translate [JUMP "a", JUMPI "b"] `shouldMissErr` ["a", "b"]

      for_ (permutations [JUMP "a", JUMPDEST "a", JUMP "b"]) $
        \instructions -> L.translate instructions `shouldMissErr` ["b"]

    it "fails on single duplicate destination" $
      L.translate [JUMPDEST "foo", JUMPDEST "foo"] `shouldDupErr` ["foo"]

    it "fails on duplicate destination in presence of non-duplicate destination" $
      for_ (permutations [JUMPDEST "foo", JUMPDEST "foo", JUMPDEST "bar"]) $
        \instructions -> L.translate instructions `shouldDupErr` ["foo"]

    it "fails on multiple duplicate destinations" $
      let instructions = [JUMPDEST "foo", JUMPDEST "bar", JUMPDEST "foo", JUMPDEST "bar"]
      in L.translate instructions `shouldDupErr` ["bar", "foo"]

    it "fails and reports both jumps without destinations and duplicate destinations" $
      let instructions = [JUMP "foo", JUMPDEST "bar", JUMPDEST "bar"]
      in L.translate instructions `shouldBe` Left (TranslateError ["foo"] ["bar"])

    it "fails and reports multiple jumps without destination and multiple duplicate destinations" $
      let instructions =
            [ JUMP "a"
            , JUMP "b"
            , JUMP "good"
            , JUMPI "c"
            , JUMPI "d"
            , JUMPI "good"
            , JUMPDEST "x"
            , JUMPDEST "x"
            , JUMPDEST "y"
            , JUMPDEST "y"
            , JUMPDEST "good"
            ]
          wildJumps = [ "a", "b", "c", "d" ]
          duplicateDests = [ "x", "y" ]

      in L.translate instructions `shouldBe` Left (TranslateError wildJumps duplicateDests)

shouldMissErr :: (Show b, Eq b) => Either TranslateError b -> [Label] -> Expectation
shouldMissErr x y = x `shouldBe` Left (TranslateError y [])

shouldDupErr :: (Show b, Eq b) => Either TranslateError b -> [Label] -> Expectation
shouldDupErr x y = x `shouldBe` Left (TranslateError [] y)
