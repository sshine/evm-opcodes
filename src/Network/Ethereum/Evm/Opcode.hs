{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module: Network.Ethereum.Evm.Opcode
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exposes the `Opcode` type for expressing Ethereum VM opcodes
-- as extracted from the EIP-150 revision of the Ethereum Yellow Paper found
-- on http://yellowpaper.io/ appendix H.2.
--
-- It also exposes the `AbstractOpcode` type for deriving various variants
-- of opcode, e.g. LabelledOpcode. This should eventually be moved to an
-- `Internal` library.

module Network.Ethereum.Evm.Opcode where

import Prelude hiding (LT, EQ, GT)

import           Control.Monad (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.DoubleWord (Word256, fromHiAndLo)
import qualified Data.Serialize.Get as Cereal
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import           Data.TinyWord (Word2, Word4)
import           Data.Word (Word8)
import           Text.Printf (printf)

-- | An Ethereum VM Opcode.
type Opcode = AbstractOpcode ()

-- | An Ethereum VM Opcode parameterised by the type of jumps. For a plain
-- opcode using the basic EVM stack-based jumps, use `Opcode` instead. This
-- type is used for defining and translating from opcodes with labelled
-- jumps.
data AbstractOpcode jumpdest =
            -- 0s: Stop and Arithmetic Operations
              STOP       -- ^ 0x00
            | ADD        -- ^ 0x01
            | MUL        -- ^ 0x02
            | SUB        -- ^ 0x03
            | DIV        -- ^ 0x04
            | SDIV       -- ^ 0x05
            | MOD        -- ^ 0x06
            | SMOD       -- ^ 0x07
            | ADDMOD     -- ^ 0x08
            | MULMOD     -- ^ 0x09
            | EXP        -- ^ 0x0a
            | SIGNEXTEND -- ^ 0x0b

            -- 10s: Comparison & Bitwise Logic Operations
            | LT     -- ^ 0x10
            | GT     -- ^ 0x11
            | SLT    -- ^ 0x12
            | SGT    -- ^ 0x13
            | EQ     -- ^ 0x14
            | ISZERO -- ^ 0x15
            | AND    -- ^ 0x16
            | OR     -- ^ 0x17
            | XOR    -- ^ 0x18
            | NOT    -- ^ 0x19
            | BYTE   -- ^ 0x1a
            | SHL    -- ^ 0x1b, https://eips.ethereum.org/EIPS/eip-145
            | SHR    -- ^ 0x1c, https://eips.ethereum.org/EIPS/eip-145
            | SAR    -- ^ 0x1d, https://eips.ethereum.org/EIPS/eip-145

            -- 20s: SHA3
            | SHA3          -- ^ 0x20

            -- 30s: Environmental Information
            | ADDRESS        -- ^ 0x30
            | BALANCE        -- ^ 0x31
            | ORIGIN         -- ^ 0x32
            | CALLER         -- ^ 0x33
            | CALLVALUE      -- ^ 0x34
            | CALLDATALOAD   -- ^ 0x35
            | CALLDATASIZE   -- ^ 0x36
            | CALLDATACOPY   -- ^ 0x37
            | CODESIZE       -- ^ 0x38
            | CODECOPY       -- ^ 0x39
            | GASPRICE       -- ^ 0x3a
            | EXTCODESIZE    -- ^ 0x3b
            | EXTCODECOPY    -- ^ 0x3c
            | RETURNDATASIZE -- ^ 0x3d, https://eips.ethereum.org/EIPS/eip-211
            | RETURNDATACOPY -- ^ 0x3e, https://eips.ethereum.org/EIPS/eip-211
            | EXTCODEHASH    -- ^ 0x3f, https://eips.ethereum.org/EIPS/eip-1052

            -- 40s: Block Information
            | BLOCKHASH   -- ^ 0x40
            | COINBASE    -- ^ 0x41
            | TIMESTAMP   -- ^ 0x42
            | NUMBER      -- ^ 0x43
            | DIFFICULTY  -- ^ 0x44
            | GASLIMIT    -- ^ 0x45
            | CHAINID     -- ^ 0x46, https://eips.ethereum.org/EIPS/eip-1344
            | SELFBALANCE -- ^ 0x47, https://eips.ethereum.org/EIPS/eip-1884

            -- 50s: Stack, Memory, Storage and Flow Operations
            | POP               -- ^ 0x50
            | MLOAD             -- ^ 0x51
            | MSTORE            -- ^ 0x52
            | MSTORE8           -- ^ 0x53
            | SLOAD             -- ^ 0x54
            | SSTORE            -- ^ 0x55
            | JUMP jumpdest     -- ^ 0x56
            | JUMPI jumpdest    -- ^ 0x57
            | PC                -- ^ 0x58
            | MSIZE             -- ^ 0x59
            | GAS               -- ^ 0x5a
            | JUMPDEST jumpdest -- ^ 0x5b

            -- 60s & 70s: Push Operations
            | PUSH !Word256 -- ^ 0x60 - 0x7f (PUSH1-PUSH32)
            | DUP !Word4    -- ^ 0x80 - 0x8f (DUP1-DUP16)
            | SWAP !Word4   -- ^ 0x90 - 0x9f (SWAP1-SWAP16)

            -- a0s: Logging Operations
            | LOG !Word2    -- ^ 0x0a - 0xa4 (LOG0-LOG4)

            -- f0s: System Operations
            | CREATE       -- ^ 0xf0
            | CALL         -- ^ 0xf1
            | CALLCODE     -- ^ 0xf2
            | RETURN       -- ^ 0xf3
            | DELEGATECALL -- ^ 0xf4, https://eips.ethereum.org/EIPS/eip-7
            | CREATE2      -- ^ 0xf5, https://eips.ethereum.org/EIPS/eip-1014
            | STATICCALL   -- ^ 0xfa
            | REVERT       -- ^ 0xfd, https://eips.ethereum.org/EIPS/eip-140
            | INVALID      -- ^ 0xfe, https://eips.ethereum.org/EIPS/eip-141
            | SELFDESTRUCT -- ^ 0xff, https://eips.ethereum.org/EIPS/eip-6
  deriving (Eq, Ord, Functor)

-- | `jump`, `jumpi` and `jumpdest` are non-parameterised `Opcode`s.
jump, jumpi, jumpdest :: Opcode
jump = JUMP ()
jumpi = JUMPI ()
jumpdest = JUMPDEST ()

-- | An `OpcodeSpecification` for a given `Opcode` contains the numeric
-- encoding of the opcode, the number of items that this opcode removes
-- from the stack (α), and the number of items added to the stack (δ).
--
-- Examples of `OpSpec`s:
--
-- > OpSpec 0x01 2 1 "add"
-- > OpSpec 0x60 0 1 "push1 255"
data OpcodeSpecification =
  OpSpec { _opcodeEncoding :: Word8 -- ^ The numeric encoding
         , _opcodeAlpha    :: Word8 -- ^ Items placed on the stack
         , _opcodeDelta    :: Word8 -- ^ Items removed from the stack
         , _opcodeName     :: Text  -- ^ A printable name
         }

-- | Given an `Opcode`, produce its `OpcodeSpecification`. For `DUP`, `SWAP`
-- and `LOG` this depends on the specific variant, and for `PUSH` it depends
-- on the constant size being pushed.
opcodeSpec :: Opcode -> OpcodeSpecification
opcodeSpec opcode = case opcode of
  -- 0s: Stop and Arithmetic Operations
  --                   Hex  α δ
  STOP       -> OpSpec 0x00 0 0 "stop"
  ADD        -> OpSpec 0x01 2 1 "add"
  MUL        -> OpSpec 0x02 2 1 "mul"
  SUB        -> OpSpec 0x03 2 1 "sub"
  DIV        -> OpSpec 0x04 2 1 "div"
  SDIV       -> OpSpec 0x05 2 1 "sdiv"
  MOD        -> OpSpec 0x06 2 1 "mod"
  SMOD       -> OpSpec 0x07 2 1 "smod"
  ADDMOD     -> OpSpec 0x08 3 1 "addmod"
  MULMOD     -> OpSpec 0x09 3 1 "mulmod"
  EXP        -> OpSpec 0x10 2 1 "exp"
  SIGNEXTEND -> OpSpec 0x11 2 1 "signextend"

  -- 10s: Comparison & Bitwise Logic Operations
  --                Hex  α δ
  LT      -> OpSpec 0x10 2 1 "lt"
  GT      -> OpSpec 0x11 2 1 "gt"
  SLT     -> OpSpec 0x12 2 1 "slt"
  SGT     -> OpSpec 0x13 2 1 "sgt"
  EQ      -> OpSpec 0x14 2 1 "eq"
  ISZERO  -> OpSpec 0x15 1 1 "iszero"
  AND     -> OpSpec 0x16 2 1 "and"
  OR      -> OpSpec 0x17 2 1 "or"
  XOR     -> OpSpec 0x18 2 1 "xor"
  NOT     -> OpSpec 0x19 1 1 "not"
  BYTE    -> OpSpec 0x1a 2 1 "byte"
  SHL     -> OpSpec 0x1b 2 1 "shl"
  SHR     -> OpSpec 0x1c 2 1 "shr"
  SAR     -> OpSpec 0x1d 2 1 "sar"

  -- 20s: SHA3
  --               Hex  α δ
  SHA3   -> OpSpec 0x20 2 1 "sha3"

  -- 30s: Environmental Information
  --     Opcode            Hex  α δ
  ADDRESS        -> OpSpec 0x30 0 1 "address"
  BALANCE        -> OpSpec 0x31 1 1 "balance"
  ORIGIN         -> OpSpec 0x32 0 1 "origin"
  CALLER         -> OpSpec 0x33 0 1 "caller"
  CALLVALUE      -> OpSpec 0x34 0 1 "callvalue"
  CALLDATALOAD   -> OpSpec 0x35 1 1 "calldataload"
  CALLDATASIZE   -> OpSpec 0x36 0 1 "calldatasize"
  CALLDATACOPY   -> OpSpec 0x37 3 0 "calldatacopy"
  CODESIZE       -> OpSpec 0x38 0 1 "codesize"
  CODECOPY       -> OpSpec 0x39 3 0 "codecopy"
  GASPRICE       -> OpSpec 0x3a 0 1 "gasprice"
  EXTCODESIZE    -> OpSpec 0x3b 1 1 "extcodesize"
  EXTCODECOPY    -> OpSpec 0x3c 4 0 "extcodecopy"
  RETURNDATASIZE -> OpSpec 0x3d 0 1 "returndatasize"
  RETURNDATACOPY -> OpSpec 0x3e 3 0 "returndatacopy"
  EXTCODEHASH    -> OpSpec 0x3f 1 1 "extcodehash"

  -- 40s: Block Information
  --                    Hex  α δ
  BLOCKHASH   -> OpSpec 0x40 1 1 "blockhash"
  COINBASE    -> OpSpec 0x41 0 1 "coinbase"
  TIMESTAMP   -> OpSpec 0x42 0 1 "timestamp"
  NUMBER      -> OpSpec 0x43 0 1 "number"
  DIFFICULTY  -> OpSpec 0x44 0 1 "difficulty"
  GASLIMIT    -> OpSpec 0x45 0 1 "gaslimit"
  CHAINID     -> OpSpec 0x46 0 1 "chainid"
  SELFBALANCE -> OpSpec 0x47 0 1 "selfbalance"

  -- 50s: Stack, Memory, Storage and Flow Operations
  --                    Hex  α δ
  POP         -> OpSpec 0x50 1 0 "pop"
  MLOAD       -> OpSpec 0x51 1 1 "mload"
  MSTORE      -> OpSpec 0x52 2 0 "mstore"
  MSTORE8     -> OpSpec 0x53 2 0 "mstore8"
  SLOAD       -> OpSpec 0x54 1 1 "sload"
  SSTORE      -> OpSpec 0x55 2 0 "sstore"
  JUMP{}      -> OpSpec 0x56 1 0 "jump"
  JUMPI{}     -> OpSpec 0x57 2 0 "jumpi"
  PC          -> OpSpec 0x58 0 1 "pc"
  MSIZE       -> OpSpec 0x59 0 1 "msize"
  GAS         -> OpSpec 0x5a 0 1 "gas"
  JUMPDEST{}  -> OpSpec 0x5b 0 0 "jumpdest"

  -- 60s & 70s: Push Operations
  PUSH n    -> let (pushEncoding, pushBytes) = push' n in
               OpSpec { _opcodeEncoding = pushEncoding
                      , _opcodeAlpha    = 0
                      , _opcodeDelta    = 1
                      , _opcodeName     = Text.concat
                          [ "push"
                          , Text.pack (show (List.length pushBytes))
                          , " "
                          , Text.pack (show n) ]
                      }

  -- 80s: Duplication Operations (DUP)
  DUP i     -> OpSpec { _opcodeEncoding = 0x80 + fromIntegral i
                      , _opcodeAlpha    = fromIntegral i + 1
                      , _opcodeDelta    = fromIntegral i + 2
                      , _opcodeName     = "dup" <> Text.pack (show (i+1))
                      }

  -- 90s: Exchange operations (SWAP)
  SWAP i    -> OpSpec { _opcodeEncoding = 0x90 + fromIntegral i
                      , _opcodeAlpha    = fromIntegral i + 1
                      , _opcodeDelta    = fromIntegral i + 1
                      , _opcodeName     = "swap" <> Text.pack (show (i+1))
                      }

  -- a0s: Logging Operations (LOG)
  LOG i     -> OpSpec { _opcodeEncoding = 0xa0 + fromIntegral i
                      , _opcodeAlpha    = fromIntegral i + 2
                      , _opcodeDelta    = 0
                      , _opcodeName     = "log" <> Text.pack (show (i+1))
                      }

  -- f0s: System Operations
  --                      Hex  α δ
  CREATE        -> OpSpec 0xf0 3 1 "create"
  CALL          -> OpSpec 0xf1 7 1 "call"
  CALLCODE      -> OpSpec 0xf2 7 1 "callcode"
  RETURN        -> OpSpec 0xf3 2 0 "return"
  DELEGATECALL  -> OpSpec 0xf4 6 1 "delegatecall"
  CREATE2       -> OpSpec 0xf5 4 1 "create2"
  STATICCALL    -> OpSpec 0xfa 6 1 "staticcall"
  REVERT        -> OpSpec 0xfd 2 0 "revert"
  INVALID       -> OpSpec 0xfe 0 0 "invalid" -- α, δ are ∅
  SELFDESTRUCT  -> OpSpec 0xff 1 0 "selfdestruct"

-- | Parse an `Opcode` from a `Word8`. In case of `PUSH` instructions, read the
-- constant being pushed from a subsequent `ByteString`.
readOpcode :: Word8 -> ByteString -> Maybe Opcode
readOpcode b bs
  | b >= 0x80 && b <= 0x8f = pure . DUP  . fromIntegral $ b - 0x80 + 1
  | b >= 0x90 && b <= 0x9f = pure . SWAP . fromIntegral $ b - 0x90 + 1
  | b >= 0xa0 && b <= 0xa4 = pure . LOG  . fromIntegral $ b - 0xa0
  | b >= 0x60 && b <= 0x7f =
    let n = fromIntegral (b - 0x60 + 1)
    in PUSH <$> word256 (BS.take n bs)
  | otherwise = case b of

  -- 0s: Stop and Arithmetic Operations
  0x00 -> pure STOP
  0x01 -> pure ADD
  0x02 -> pure MUL
  0x03 -> pure SUB
  0x04 -> pure DIV
  0x05 -> pure SDIV
  0x06 -> pure MOD
  0x07 -> pure SMOD
  0x08 -> pure ADDMOD
  0x09 -> pure MULMOD
  0x0a -> pure EXP
  0x0b -> pure SIGNEXTEND

  -- 10s: Comparison & Bitwise Logic Operations
  0x10 -> pure LT
  0x11 -> pure GT
  0x12 -> pure SLT
  0x13 -> pure SGT
  0x14 -> pure EQ
  0x15 -> pure ISZERO
  0x16 -> pure AND
  0x17 -> pure OR
  0x18 -> pure XOR
  0x19 -> pure NOT
  0x1a -> pure BYTE
  0x1b -> pure SHL
  0x1c -> pure SHR
  0x1d -> pure SAR

  -- 20s: SHA3
  0x20 -> pure SHA3

  -- 30s: Environmental Information
  0x30 -> pure ADDRESS
  0x31 -> pure BALANCE
  0x32 -> pure ORIGIN
  0x33 -> pure CALLER
  0x34 -> pure CALLVALUE
  0x35 -> pure CALLDATALOAD
  0x36 -> pure CALLDATASIZE
  0x37 -> pure CALLDATACOPY
  0x38 -> pure CODESIZE
  0x39 -> pure CODECOPY
  0x3a -> pure GASPRICE
  0x3b -> pure EXTCODESIZE
  0x3c -> pure EXTCODECOPY
  0x3d -> pure RETURNDATASIZE
  0x3e -> pure RETURNDATACOPY
  0x3f -> pure EXTCODEHASH

  -- 40s: Block Information
  0x40 -> pure BLOCKHASH
  0x41 -> pure COINBASE
  0x42 -> pure TIMESTAMP
  0x43 -> pure NUMBER
  0x44 -> pure DIFFICULTY
  0x45 -> pure GASLIMIT
  0x46 -> pure CHAINID
  0x47 -> pure SELFBALANCE

  -- 50s: Stack, Memory, Storage and Flow Operations
  0x50 -> pure POP
  0x51 -> pure MLOAD
  0x52 -> pure MSTORE
  0x53 -> pure MSTORE8
  0x54 -> pure SLOAD
  0x55 -> pure SSTORE
  0x56 -> pure jump
  0x57 -> pure jumpi
  0x58 -> pure PC
  0x59 -> pure MSIZE
  0x5a -> pure GAS
  0x5b -> pure jumpdest

  -- f0s: System Operations
  0xf0 -> pure CREATE
  0xf1 -> pure CALL
  0xf2 -> pure CALLCODE
  0xf3 -> pure RETURN
  0xf4 -> pure DELEGATECALL
  0xf5 -> pure CREATE2
  0xfa -> pure STATICCALL
  0xfd -> pure REVERT
  0xfe -> pure INVALID
  0xff -> pure SELFDESTRUCT

  _    -> Nothing

-- | Read a Word256 from a ByteString.
word256 :: ByteString -> Maybe Word256
word256 xs =
  case Cereal.runGet m (padLeft 32 xs) of
    Left _ -> Nothing
    Right x -> pure x
  where
    m = do
      a <- Cereal.getWord64be
      b <- Cereal.getWord64be
      c <- Cereal.getWord64be
      d <- Cereal.getWord64be
      pure $ fromHiAndLo (fromHiAndLo a b) (fromHiAndLo c d)

    padLeft :: Int -> ByteString -> ByteString
    padLeft n xs = BS.replicate (n - BS.length xs) 0 <> xs

-- | Convert any `AbstractOpcode` into an un-parameterised `Opcode`.
concrete :: AbstractOpcode a -> Opcode
concrete = void

-- | Show `Opcode` as `Text`.
opcodeText :: Opcode -> Text
opcodeText = _opcodeName . opcodeSpec

-- | Show `Opcode` as `String`.
instance Show a => Show (AbstractOpcode a) where
  show opcode = Text.unpack (opcodeText (concrete opcode)) <> show' opcode
    where
      show' (JUMP a) = " " <> show a
      show' (JUMPI a) = " " <> show a
      show' (JUMPDEST a) = " " <> show a
      show' opcode = ""

-- | Calculate the size in bytes of an encoded opcode. The only `Opcode`
-- that uses more than one byte is `PUSH`. Sizes are trivially determined
-- for only `Opcode` with unlabelled jumps, since we cannot know e.g. where
-- the label of a `LabelledOpcode` points to before code generation has
-- completed.
opcodeSize :: Num i => Opcode -> i
opcodeSize (PUSH n) = List.genericLength . uncurry (:) $ push' n
opcodeSize _opcode = 1

-- | Pretty-print an `Opcode` as a hexadecimal code (`Text`).
ppHex :: Opcode -> Text
ppHex = Text.pack . showHex

-- | Pretty-print an `Opcode` as a hexadecimal code (`String`).
showHex :: Opcode -> String
showHex (PUSH n) = List.concatMap (printf "%02x") . uncurry (:) $ push' n
showHex opcode = printf "%02x" . _opcodeEncoding . opcodeSpec $ opcode

-- | Convert the constant argument of a `PUSH` to the opcode encoding
-- (0x60--0x7f) and its constant split into `Word8` segments.
push' :: Word256 -> (Word8, [Word8])
push' i | i < 256 = (0x60, [fromIntegral i])
push' i = (opcode + 1, arg <> [fromIntegral i])
  where (opcode, arg) = push' (i `quot` 256)
