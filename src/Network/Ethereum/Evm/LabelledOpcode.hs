{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Network.Ethereum.Evm.LabelledOpcode
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exposes the `LabelledOpcode` type for expressing Ethereum VM
-- opcodes with labelled jumps. Plain Ethereum VM Opcodes are difficult to
-- compose because jumping to a fixed location in program memory creates a
-- non-trivial constraint in the code-generator:
--
-- With `Opcode` the address is pushed to the stack via `PUSH`, but the
-- offset to the `JUMPDEST` depends on all occurrences of `PUSH` prior to
-- the label, including the `PUSH` to the label itself.
--
-- With `LabelledOpcode` one can express named labels and not worry about
-- calculating the positions of labels until a late stage.

module Network.Ethereum.Evm.LabelledOpcode where

import Data.List (null, filter, group, sort)
import Data.Text (Text)

import qualified Data.Map as M
import           Data.Map (Map)

import Data.Maybe (mapMaybe)
import Control.Monad (forM)

import Network.Ethereum.Evm.Opcode (AbstractOpcode(..), opcodeSize, jumpdest, concrete)
import Network.Ethereum.Evm.PositionedOpcode (Position, PositionedOpcode, jumpSize)
import Network.Ethereum.Evm.OpcodeTraversals

-- | For now, all labels are `Text`.
type Label = Text

-- | LabelledOpcodes: `JUMP "name"`, `JUMPI "name"` and `JUMPDEST "name"`.
-- All other Opcodes remain the same.
type LabelledOpcode = AbstractOpcode Label

-- | Translation of `LabelledOpcode` into `PositionedOpcode` may fail if
-- either a jump is made to a label that doesn't occur (`Missing`), or a
-- label occurs twice in different positions (`Duplicate`).
data TranslateError
  = Missing [Label]
  | Duplicate [Label]
  deriving (Eq, Show)

translate :: [LabelledOpcode] -> Either TranslateError [PositionedOpcode]
translate opcodes = do
  labelMap <- labelPositions opcodes
  traverse (mapOpcodeM (om labelMap)) opcodes
  where
    om labelMap = OpcodeMapperM
      { mapOnJump = \label -> JUMP <$> positionFor label labelMap
      , mapOnJumpi = \label -> JUMPI <$> positionFor label labelMap
      , mapOnJumpDest = \label -> JUMPDEST <$> positionFor label labelMap
      , mapOnOther = \label -> pure Nothing
      }

    positionFor :: Label -> Map Label Position -> Either TranslateError Position
    positionFor label labelMap = case M.lookup label labelMap of
      Just pos -> Right pos
      Nothing -> Left (Missing [label])

labelPositions :: [LabelledOpcode] -> Either TranslateError (Map Label Position)
labelPositions opcodes
  | not (null (jumps `missing` dests)) = Left (Missing (jumps `missing` dests))
  | not (null (duplicates dests)) = Left (Duplicate (duplicates dests))
  | otherwise = Right (fixpoint M.empty)
  where
    jumps :: [Label]
    jumps = flip mapMaybe opcodes $ \case
      JUMP label -> pure label
      JUMPI label -> pure label
      _ -> Nothing

    dests :: [Label]
    dests = flip mapMaybe opcodes $ \case
      JUMPDEST label -> Just label
      _ -> Nothing

    fixpoint :: Map Label Position -> Map Label Position
    fixpoint labelMap =
      case foldr align (0, labelMap, True) opcodes of
        (_, labelMap', True) -> labelMap'
        (_, labelMap', False) -> fixpoint labelMap'

    align :: LabelledOpcode
          -> (Position, Map Label Position, Bool)
          -> (Position, Map Label Position, Bool)
    align (JUMPDEST label) (curBytePos, labelMap, done) =
      let aligned = (== Just curBytePos) $ M.lookup label labelMap
      in ( curBytePos + opcodeSize jumpdest
         , M.insert label curBytePos labelMap
         , aligned && done )

    align (JUMP label) (curBytePos, labelMap, done) =
      let (bytePos, aligned) = maybe (0,False) (,True) $ M.lookup label labelMap
      in ( curBytePos + jumpSize bytePos
         , labelMap
         , aligned && done )

    align (JUMPI label) (curBytePos, labelMap, done) =
      let (bytePos, aligned) = maybe (0,False) (,True) $ M.lookup label labelMap
      in ( curBytePos + jumpSize bytePos
         , labelMap
         , aligned && done )

    align opcode (curBytePos, labelMap, done) =
      ( curBytePos + opcodeSize (concrete opcode)
      , labelMap
      , done )

-- | Complete difference: Get the elements of `xs` that do not occur in `ys`.
missing :: Eq a => [a] -> [a] -> [a]
missing xs ys = filter (`notElem` ys) xs

-- | Get a sublist of elements that occur more than once.
duplicates :: Ord a => [a] -> [a]
duplicates = concatMap (take 1) . filter ((> 1) . length) . group . sort
