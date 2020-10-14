{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: EVM.Opcode.Labelled
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exposes the 'LabelledOpcode' type for expressing Ethereum VM
-- opcodes with labelled jumps. Plain Ethereum VM Opcodes are not so ergonomic
-- because one has to know the exact byte offset of the target 'JUMPDEST'.
--
-- With 'Opcode' the byte offset is pushed to the stack via 'PUSH', but the
-- offset to the 'JUMPDEST' depends on all occurrences of 'PUSH' prior to
-- the label, including the 'PUSH' to the label itself.

module EVM.Opcode.Labelled
  ( Label
  , LabelledOpcode
  , TranslateError(..)
  , translate
  ) where

import           Data.List (group, sort)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)

import           EVM.Opcode (Opcode'(..), opcodeSize, jumpdest, concrete, jumpAnnot, jumpdestAnnot)
import           EVM.Opcode.Positional (Position, PositionalOpcode, jumpSize)
import           EVM.Opcode.Traversal (OpcodeMapper(..), mapOpcodeM)

-- | For now, all labels are 'Text'.
type Label = Text

-- | 'LabelledOpcode's use 'Label' to represent jumps.
--
-- In particular, @'JUMP' "name"@, @'JUMPI' "name"@ and @'JUMPDEST' "name"@.
--
-- All other opcodes remain the same.
type LabelledOpcode = Opcode' Label

-- | Translation of a 'LabelledOpcode' into 'PositionalOpcode' may fail if
-- either a jump is made to a labelled 'JUMPDEST' that does not occur (is
-- missing), or a labelled 'JUMPDEST' occurs twice in different positions (is
-- a duplicate).
data TranslateError = TranslateError
  { translateErrorMissingJumpdests   :: [Label]
  , translateErrorDuplicateJumpdests :: [Label]
  } deriving (Eq, Show)

-- | Translate a 'LabelledOpcode' into a list of 'PositionalOpcode' by
-- replacing the labels with absolute positions. The positions are calculated
-- using a fixed-point algorithm so that the (variable) size of a jump itself
-- is accounted for.
--
-- Labelled jumps don't have a size defined, but the size of a positional jump
-- depends on the address being jumped to. So for example, if jumping to the
-- 'JUMPDEST' on the 256th position in a @['LabelledOpcode']@, this requires
-- a 'PUSH2' instruction which uses an additional byte, which pushes the
-- 'JUMPDEST' one byte ahead.

translate :: [LabelledOpcode] -> Either TranslateError [PositionalOpcode]
translate opcodes = do
  labelMap <- labelPositions opcodes
  traverse (mapOpcodeM (myMapper (lookup' labelMap))) opcodes
  where
    myMapper :: (Label -> Either TranslateError Position)
             -> OpcodeMapper (Either TranslateError) Label Position
    myMapper f = OpcodeMapper
      { mapOnJump = fmap JUMP . f
      , mapOnJumpi = fmap JUMPI . f
      , mapOnJumpdest = fmap JUMPDEST .f
      , mapOnOther = const (pure Nothing)
      }

    lookup' labelMap label = case Map.lookup label labelMap of
      Just pos -> Right pos
      Nothing -> Left (TranslateError [label] [])

-- | Extract a @'Map' 'Label' 'Position'@ that describes where each 'JUMPDEST'
-- is located, taking into account the sizes of all prior opcodes.
labelPositions :: [LabelledOpcode] -> Either TranslateError (Map Label Position)
labelPositions opcodes =
  case (jumps `missing` dests, duplicate dests) of
    ([], []) -> Right (fixpoint opcodes Map.empty)
    (missing', duplicate') -> Left (TranslateError missing' duplicate')
  where
    jumps :: [Label]
    jumps = mapMaybe jumpAnnot opcodes

    dests :: [Label]
    dests = mapMaybe jumpdestAnnot opcodes

-- | Update the position of every 'JUMPDEST'.
--
-- Do this by keeping track of the current position.
--
-- For jumps, increase the current position by however much
--
-- FIXME: Use 'Data.Function.fix'.
fixpoint :: [LabelledOpcode] -> Map Label Position -> Map Label Position
fixpoint opcodes labelMap =
  case step opcodes labelMap of
    (True, _, labelMap') -> labelMap'
    (False, _, labelMap') -> fixpoint opcodes labelMap'

step :: [LabelledOpcode] -> Map Label Position -> (Bool, Position, Map Label Position)
step opcodes labelMap = foldl align (True, 0, labelMap) opcodes

align :: (Bool, Position, Map Label Position) -> LabelledOpcode
      -> (Bool, Position, Map Label Position)
align (done, currentBytePos, labelMap) (JUMPDEST label) =
  let aligned = Map.lookup label labelMap == Just currentBytePos
  in ( done && aligned
     , currentBytePos + opcodeSize jumpdest
     , Map.insert label currentBytePos labelMap
     )

align (done, currentBytePos, labelMap) (jumpAnnot -> Just label) =
  case Map.lookup label labelMap of
    Just bytePos -> ( done, currentBytePos + jumpSize bytePos, labelMap )
    Nothing      -> ( False, currentBytePos + jumpSize 0,      labelMap )

align (done, currentBytePos, labelMap) opcode =
  ( done, currentBytePos + opcodeSize (concrete opcode), labelMap )

-- | Complete difference: Get the elements of 'xs' that do not occur in 'ys'.
missing :: Ord a => [a] -> [a] -> [a]
missing xs ys = Set.toList (Set.difference (Set.fromList xs) (Set.fromList ys))

-- | Get a sublist of elements that occur more than once.
duplicate :: Ord a => [a] -> [a]
duplicate = concatMap (take 1) . filter ((> 1) . length) . group . sort
