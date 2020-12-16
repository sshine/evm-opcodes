{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module: EVM.Opcode
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exposes the 'Opcode'' and 'Opcode' types for expressing
-- Ethereum VM opcodes as extracted from the Ethereum Yellow Paper with
-- amendments from various EIPs. The Yellow Paper is available at:
--
-- https://ethereum.github.io/yellowpaper/paper.pdf
--
-- The list of opcodes is found in appendix H.2.
--
-- But it is not always up-to-date, so keeping track of EIPs that add or
-- modify instructions is necessary. See comments in this module for the
-- references to these additions.

module EVM.Opcode
  ( -- Types
    Opcode
  , Opcode'(..)
  , OpcodeSpec(..)
  , opcodeSpec

    -- Pseudo-instructions
  , jump
  , jumpi
  , jumpdest

    -- Extraction
  , jumpAnnot
  , jumpdestAnnot

    -- Parse and validate
  , isDUP
  , isSWAP
  , isLOG
  , isPUSH
  , readDUP
  , readSWAP
  , readLOG
  , readPUSH
  , readOp

    -- Conversion and printing
  , concrete
  , opcodeText
  , opcodeSize
  , toHex
  , pack
  , toBytes

    -- Pattern synonyms
  , pattern DUP1,  pattern DUP2,  pattern DUP3,  pattern DUP4
  , pattern DUP5,  pattern DUP6,  pattern DUP7,  pattern DUP8
  , pattern DUP9,  pattern DUP10, pattern DUP11, pattern DUP12
  , pattern DUP13, pattern DUP14, pattern DUP15, pattern DUP16

  , pattern SWAP1,  pattern SWAP2,  pattern SWAP3,  pattern SWAP4
  , pattern SWAP5,  pattern SWAP6,  pattern SWAP7,  pattern SWAP8
  , pattern SWAP9,  pattern SWAP10, pattern SWAP11, pattern SWAP12
  , pattern SWAP13, pattern SWAP14, pattern SWAP15, pattern SWAP16

  , pattern LOG0, pattern LOG1, pattern LOG2, pattern LOG3, pattern LOG4
  ) where

import Prelude hiding (LT, EQ, GT)

import           Control.Applicative ((<|>))
import           Control.Monad (void, guard)
import           Data.Bits (shift)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.DoubleWord (Word256, fromHiAndLo)
import           Data.Maybe (isJust)
import qualified Data.Serialize.Get as Cereal
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import           Data.Word (Word8, Word64)
import           Text.Printf (printf)

-- | An 'Opcode' is a plain Ethereum VM Opcode.
type Opcode = Opcode' ()

-- | An 'Opcode'' is an Ethereum VM Opcode with parameterised jumps.
--
-- For a plain opcode using the basic EVM stack-based jumps, use 'Opcode'
-- instead. This type is used for defining and translating from opcodes
-- with labelled jumps.
data Opcode' j
  -- 0s: Stop and Arithmetic Operations
  = STOP              -- ^ 0x00
  | ADD               -- ^ 0x01
  | MUL               -- ^ 0x02
  | SUB               -- ^ 0x03
  | DIV               -- ^ 0x04
  | SDIV              -- ^ 0x05
  | MOD               -- ^ 0x06
  | SMOD              -- ^ 0x07
  | ADDMOD            -- ^ 0x08
  | MULMOD            -- ^ 0x09
  | EXP               -- ^ 0x0a
  | SIGNEXTEND        -- ^ 0x0b

  -- 10s: Comparison & Bitwise Logic Operations
  | LT                -- ^ 0x10
  | GT                -- ^ 0x11
  | SLT               -- ^ 0x12
  | SGT               -- ^ 0x13
  | EQ                -- ^ 0x14
  | ISZERO            -- ^ 0x15
  | AND               -- ^ 0x16
  | OR                -- ^ 0x17
  | XOR               -- ^ 0x18
  | NOT               -- ^ 0x19
  | BYTE              -- ^ 0x1a
  | SHL               -- ^ 0x1b, https://eips.ethereum.org/EIPS/eip-145
  | SHR               -- ^ 0x1c, https://eips.ethereum.org/EIPS/eip-145
  | SAR               -- ^ 0x1d, https://eips.ethereum.org/EIPS/eip-145

  -- 20s: SHA3
  | SHA3              -- ^ 0x20

  -- 30s: Environmental Information
  | ADDRESS           -- ^ 0x30
  | BALANCE           -- ^ 0x31
  | ORIGIN            -- ^ 0x32
  | CALLER            -- ^ 0x33
  | CALLVALUE         -- ^ 0x34
  | CALLDATALOAD      -- ^ 0x35
  | CALLDATASIZE      -- ^ 0x36
  | CALLDATACOPY      -- ^ 0x37
  | CODESIZE          -- ^ 0x38
  | CODECOPY          -- ^ 0x39
  | GASPRICE          -- ^ 0x3a
  | EXTCODESIZE       -- ^ 0x3b
  | EXTCODECOPY       -- ^ 0x3c
  | RETURNDATASIZE    -- ^ 0x3d, https://eips.ethereum.org/EIPS/eip-211
  | RETURNDATACOPY    -- ^ 0x3e, https://eips.ethereum.org/EIPS/eip-211
  | EXTCODEHASH       -- ^ 0x3f, https://eips.ethereum.org/EIPS/eip-1052

  -- 40s: Block Information
  | BLOCKHASH         -- ^ 0x40
  | COINBASE          -- ^ 0x41
  | TIMESTAMP         -- ^ 0x42
  | NUMBER            -- ^ 0x43
  | DIFFICULTY        -- ^ 0x44
  | GASLIMIT          -- ^ 0x45
  | CHAINID           -- ^ 0x46, https://eips.ethereum.org/EIPS/eip-1344
  | SELFBALANCE       -- ^ 0x47, https://eips.ethereum.org/EIPS/eip-1884

  -- 50s: Stack, Memory, Storage and Flow Operations
  | POP               -- ^ 0x50
  | MLOAD             -- ^ 0x51
  | MSTORE            -- ^ 0x52
  | MSTORE8           -- ^ 0x53
  | SLOAD             -- ^ 0x54
  | SSTORE            -- ^ 0x55
  | JUMP j            -- ^ 0x56
  | JUMPI j           -- ^ 0x57
  | PC                -- ^ 0x58
  | MSIZE             -- ^ 0x59
  | GAS               -- ^ 0x5a
  | JUMPDEST j        -- ^ 0x5b

  -- 60s & 70s: Push Operations
  | PUSH !Word256     -- ^ 0x60 - 0x7f (PUSH1 - PUSH32)
  | DUP !Ord16         -- ^ 0x80 - 0x8f ('DUP1' - 'DUP16')
  | SWAP !Ord16        -- ^ 0x90 - 0x9f ('SWAP1' - 'SWAP16')

  -- a0s: Logging Operations
  | LOG !Ord5         -- ^ 0xa0 - 0xa4 ('LOG0' - 'LOG4')

  -- f0s: System Operations
  | CREATE            -- ^ 0xf0
  | CALL              -- ^ 0xf1
  | CALLCODE          -- ^ 0xf2
  | RETURN            -- ^ 0xf3
  | DELEGATECALL      -- ^ 0xf4, https://eips.ethereum.org/EIPS/eip-7
  | CREATE2           -- ^ 0xf5, https://eips.ethereum.org/EIPS/eip-1014
  | STATICCALL        -- ^ 0xfa
  | REVERT            -- ^ 0xfd, https://eips.ethereum.org/EIPS/eip-140
  | INVALID           -- ^ 0xfe, https://eips.ethereum.org/EIPS/eip-141
  | SELFDESTRUCT      -- ^ 0xff, https://eips.ethereum.org/EIPS/eip-6
  deriving (Eq, Ord, Functor)

-- | 'jump' is a plain parameterless 'Opcode'.
jump :: Opcode
jump = JUMP ()

-- | 'jumpi' is a plain parameterless 'Opcode'.
jumpi :: Opcode
jumpi = JUMPI ()

-- | 'jumpdest' is a plain parameterless 'Opcode'.
jumpdest :: Opcode
jumpdest = JUMPDEST ()

-- | Extract the @a@ from a @'JUMP' a@ or a @'JUMPI' a@.
jumpAnnot :: Opcode' a -> Maybe a
jumpAnnot = \case
  JUMP a -> Just a
  JUMPI a -> Just a
  _ -> Nothing

-- | Extract the @a@ from a @'JUMPDEST' a@.
jumpdestAnnot :: Opcode' a -> Maybe a
jumpdestAnnot = \case
  JUMPDEST a -> Just a
  _ -> Nothing

-- | Convert an 'Ord5' or an 'Ord16' to a 'Word8'.
toWord8 :: Enum e => e -> Word8
toWord8 = fromIntegral . fromEnum

-- | Convert a 'Word8' to an 'Ord5' or an 'Ord16'.
fromWord8 :: Enum e => Word8 -> e
fromWord8 = toEnum . fromIntegral

-- | Convenience type of cardinality 5 for 'LOG'.
data Ord5
  = Ord5_0
  | Ord5_1
  | Ord5_2
  | Ord5_3
  | Ord5_4
  deriving (Eq, Ord, Enum, Bounded)

-- | Convenience type of cardinality 16 for 'DUP' and 'SWAP'.
data Ord16
  = Ord16_1
  | Ord16_2
  | Ord16_3
  | Ord16_4
  | Ord16_5
  | Ord16_6
  | Ord16_7
  | Ord16_8
  | Ord16_9
  | Ord16_10
  | Ord16_11
  | Ord16_12
  | Ord16_13
  | Ord16_14
  | Ord16_15
  | Ord16_16
  deriving (Eq, Ord, Enum, Bounded)

-- | An 'OpcodeSpec' for a given 'Opcode' contains the numeric encoding of the
-- opcode, the number of items that this opcode removes from the stack (α),
-- and the number of items added to the stack (δ). These values are documented
-- in the Ethereum Yellow Paper.
--
-- Examples of 'OpcodeSpec's:
--
-- > --         Hex  α δ
-- > OpcodeSpec 0x01 2 1 "add"
-- > OpcodeSpec 0x60 0 1 "push1 255"
-- > OpcodeSpec 0x61 0 1 "push2 256"
data OpcodeSpec = OpcodeSpec
  { opcodeEncoding :: !Word8 -- ^ Numeric encoding of opcode
  , opcodeAlpha    :: !Word8 -- ^ Number of items opcode places on stack (α)
  , opcodeDelta    :: !Word8 -- ^ Number of items opcode removes from stack (δ)
  , opcodeName     :: !Text  -- ^ Printable name for opcode, e.g. @"add"@
  } deriving (Eq, Show)

-- | Given an 'Opcode'', produce its 'OpcodeSpec'.
--
-- For 'DUP', 'SWAP' and 'LOG' this depends on the specific variant, and for
-- 'PUSH' it depends on the size of the constant being pushed.
opcodeSpec :: Opcode' j -> OpcodeSpec
opcodeSpec opcode = case opcode of
  -- 0s: Stop and Arithmetic Operations
  --                   Hex  α δ
  STOP       -> OpcodeSpec 0x00 0 0 "stop"
  ADD        -> OpcodeSpec 0x01 2 1 "add"
  MUL        -> OpcodeSpec 0x02 2 1 "mul"
  SUB        -> OpcodeSpec 0x03 2 1 "sub"
  DIV        -> OpcodeSpec 0x04 2 1 "div"
  SDIV       -> OpcodeSpec 0x05 2 1 "sdiv"
  MOD        -> OpcodeSpec 0x06 2 1 "mod"
  SMOD       -> OpcodeSpec 0x07 2 1 "smod"
  ADDMOD     -> OpcodeSpec 0x08 3 1 "addmod"
  MULMOD     -> OpcodeSpec 0x09 3 1 "mulmod"
  EXP        -> OpcodeSpec 0x0a 2 1 "exp"
  SIGNEXTEND -> OpcodeSpec 0x0b 2 1 "signextend"

  -- 10s: Comparison & Bitwise Logic Operations
  --                Hex  α δ
  LT         -> OpcodeSpec 0x10 2 1 "lt"
  GT         -> OpcodeSpec 0x11 2 1 "gt"
  SLT        -> OpcodeSpec 0x12 2 1 "slt"
  SGT        -> OpcodeSpec 0x13 2 1 "sgt"
  EQ         -> OpcodeSpec 0x14 2 1 "eq"
  ISZERO     -> OpcodeSpec 0x15 1 1 "iszero"
  AND        -> OpcodeSpec 0x16 2 1 "and"
  OR         -> OpcodeSpec 0x17 2 1 "or"
  XOR        -> OpcodeSpec 0x18 2 1 "xor"
  NOT        -> OpcodeSpec 0x19 1 1 "not"
  BYTE       -> OpcodeSpec 0x1a 2 1 "byte"
  SHL        -> OpcodeSpec 0x1b 2 1 "shl"
  SHR        -> OpcodeSpec 0x1c 2 1 "shr"
  SAR        -> OpcodeSpec 0x1d 2 1 "sar"

  -- 20s: SHA3
  --               Hex  α δ
  SHA3       -> OpcodeSpec 0x20 2 1 "sha3"

  -- 30s: Environmental Information
  --     Opcode            Hex  α δ
  ADDRESS        -> OpcodeSpec 0x30 0 1 "address"
  BALANCE        -> OpcodeSpec 0x31 1 1 "balance"
  ORIGIN         -> OpcodeSpec 0x32 0 1 "origin"
  CALLER         -> OpcodeSpec 0x33 0 1 "caller"
  CALLVALUE      -> OpcodeSpec 0x34 0 1 "callvalue"
  CALLDATALOAD   -> OpcodeSpec 0x35 1 1 "calldataload"
  CALLDATASIZE   -> OpcodeSpec 0x36 0 1 "calldatasize"
  CALLDATACOPY   -> OpcodeSpec 0x37 3 0 "calldatacopy"
  CODESIZE       -> OpcodeSpec 0x38 0 1 "codesize"
  CODECOPY       -> OpcodeSpec 0x39 3 0 "codecopy"
  GASPRICE       -> OpcodeSpec 0x3a 0 1 "gasprice"
  EXTCODESIZE    -> OpcodeSpec 0x3b 1 1 "extcodesize"
  EXTCODECOPY    -> OpcodeSpec 0x3c 4 0 "extcodecopy"
  RETURNDATASIZE -> OpcodeSpec 0x3d 0 1 "returndatasize"
  RETURNDATACOPY -> OpcodeSpec 0x3e 3 0 "returndatacopy"
  EXTCODEHASH    -> OpcodeSpec 0x3f 1 1 "extcodehash"

  -- 40s: Block Information
  --                    Hex  α δ
  BLOCKHASH      -> OpcodeSpec 0x40 1 1 "blockhash"
  COINBASE       -> OpcodeSpec 0x41 0 1 "coinbase"
  TIMESTAMP      -> OpcodeSpec 0x42 0 1 "timestamp"
  NUMBER         -> OpcodeSpec 0x43 0 1 "number"
  DIFFICULTY     -> OpcodeSpec 0x44 0 1 "difficulty"
  GASLIMIT       -> OpcodeSpec 0x45 0 1 "gaslimit"
  CHAINID        -> OpcodeSpec 0x46 0 1 "chainid"
  SELFBALANCE    -> OpcodeSpec 0x47 0 1 "selfbalance"

  -- 50s: Stack, Memory, Storage and Flow Operations
  --                    Hex  α δ
  POP            -> OpcodeSpec 0x50 1 0 "pop"
  MLOAD          -> OpcodeSpec 0x51 1 1 "mload"
  MSTORE         -> OpcodeSpec 0x52 2 0 "mstore"
  MSTORE8        -> OpcodeSpec 0x53 2 0 "mstore8"
  SLOAD          -> OpcodeSpec 0x54 1 1 "sload"
  SSTORE         -> OpcodeSpec 0x55 2 0 "sstore"
  JUMP{}         -> OpcodeSpec 0x56 1 0 "jump"
  JUMPI{}        -> OpcodeSpec 0x57 2 0 "jumpi"
  PC             -> OpcodeSpec 0x58 0 1 "pc"
  MSIZE          -> OpcodeSpec 0x59 0 1 "msize"
  GAS            -> OpcodeSpec 0x5a 0 1 "gas"
  JUMPDEST{}     -> OpcodeSpec 0x5b 0 0 "jumpdest"

  -- 60s & 70s: Push Operations
  PUSH n ->
    let (pushHex, pushConst) = push' n
    in OpcodeSpec { opcodeEncoding = pushHex
                  , opcodeAlpha    = 0
                  , opcodeDelta    = 1
                  , opcodeName     = Text.concat
                    [ "push"
                    , Text.pack (show (List.length pushConst))
                    , " "
                    , Text.pack (show n) ]
                  }

  -- 80s: Duplication Operations (DUP)
  DUP i ->
    let wi = toWord8 i
    in OpcodeSpec { opcodeEncoding = 0x80 + wi
                  , opcodeAlpha    = wi + 1
                  , opcodeDelta    = wi + 2
                  , opcodeName     = "dup" <> Text.pack (show (wi + 1))
                  }

  -- 90s: Exchange operations (SWAP)
  SWAP i ->
    let wi = toWord8 i
    in OpcodeSpec { opcodeEncoding = 0x90 + wi
                  , opcodeAlpha    = wi + 1
                  , opcodeDelta    = wi + 1
                  , opcodeName     = "swap" <> Text.pack (show (wi + 1))
                  }

  -- a0s: Logging Operations (LOG)
  LOG i ->
    let wi = toWord8 i
    in OpcodeSpec { opcodeEncoding = 0xa0 + wi
                  , opcodeAlpha    = wi + 2
                  , opcodeDelta    = 0
                  , opcodeName     = "log" <> Text.pack (show wi)
                  }

  -- f0s: System Operations
  --                      Hex  α δ
  CREATE       -> OpcodeSpec 0xf0 3 1 "create"
  CALL         -> OpcodeSpec 0xf1 7 1 "call"
  CALLCODE     -> OpcodeSpec 0xf2 7 1 "callcode"
  RETURN       -> OpcodeSpec 0xf3 2 0 "return"
  DELEGATECALL -> OpcodeSpec 0xf4 6 1 "delegatecall"
  CREATE2      -> OpcodeSpec 0xf5 4 1 "create2"
  STATICCALL   -> OpcodeSpec 0xfa 6 1 "staticcall"
  REVERT       -> OpcodeSpec 0xfd 2 0 "revert"
  INVALID      -> OpcodeSpec 0xfe 0 0 "invalid" -- α, δ are ∅
  SELFDESTRUCT -> OpcodeSpec 0xff 1 0 "selfdestruct"

-- | Determine if a byte represents a 'DUP' opcode ('DUP1' -- 'DUP16').
isDUP  :: Word8 -> Bool
isDUP = isJust . readDUP

-- | Determine if a byte represents a 'SWAP' opcode ('SWAP1' -- 'SWAP16').
isSWAP  :: Word8 -> Bool
isSWAP = isJust . readSWAP

-- | Determine if a byte represents a 'LOG' opcode ('LOG1' -- 'LOG4').
isLOG  :: Word8 -> Bool
isLOG = isJust . readLOG

-- | Determine if a byte represents a 'PUSH' opcode.
isPUSH :: Word8 -> ByteString -> Bool
isPUSH b bs = isJust (readPUSH b bs)

-- | Read a 'DUP' opcode ('DUP1' -- 'DUP16') safely.
readDUP :: Word8 -> Maybe Opcode
readDUP b = do
  guard (b >= 0x80 && b <= 0x8f)
  pure (DUP (fromWord8 (b - 0x80)))

-- | Read a 'SWAP' opcode ('SWAP1' -- 'SWAP16') safely.
readSWAP :: Word8 -> Maybe Opcode
readSWAP b = do
  guard (b >= 0x90 && b <= 0x9f)
  pure (SWAP (fromWord8 (b - 0x90)))

-- | Read a 'LOG' opcode ('LOG1' -- 'LOG4') safely.
readLOG :: Word8 -> Maybe Opcode
readLOG b = do
  guard (b >= 0xa0 && b <= 0xa4)
  pure (LOG (fromWord8 (b - 0xa0)))

-- | Read a 'PUSH' opcode safely.
readPUSH :: Word8 -> ByteString -> Maybe Opcode
readPUSH b bs = do
  guard (b >= 0x60 && b <= 0x7f)
  let n = fromIntegral (b - 0x60 + 1)
  PUSH <$> word256 (BS.take n bs)

-- | Parse an 'Opcode' from a 'Word8'. In case of 'PUSH' instructions, read the
-- constant being pushed from a subsequent 'ByteString'.
readOp :: Word8 -> ByteString -> Maybe Opcode
readOp word bs
    = readDUP word
  <|> readSWAP word
  <|> readLOG word
  <|> readPUSH word bs
  <|> case word of
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

    -- Unknown
    _    -> Nothing

-- | Get a 'Word256' from a 'ByteString'
--
-- Pads the ByteString with NULs up to 32 bytes.
word256 :: ByteString -> Maybe Word256
word256 = eitherToMaybe . getWord256 . padLeft 32
  where
    getWord256 :: ByteString -> Either String Word256
    getWord256 = Cereal.runGet $
      fromWord64s <$> Cereal.getWord64be
                  <*> Cereal.getWord64be
                  <*> Cereal.getWord64be
                  <*> Cereal.getWord64be

    fromWord64s :: Word64 -> Word64 -> Word64 -> Word64 -> Word256
    fromWord64s a b c d = fromHiAndLo (fromHiAndLo a b) (fromHiAndLo c d)

    padLeft :: Int -> ByteString -> ByteString
    padLeft n xs = BS.replicate (n - BS.length xs) 0 <> xs

    eitherToMaybe :: Either e a -> Maybe a
    eitherToMaybe = either (const Nothing) pure

-- | Convert any 'Opcode' into an un-parameterised 'Op'.
concrete :: Opcode' a -> Opcode
concrete = void

-- | Show 'Opcode' as 'Text'.
opcodeText :: Opcode -> Text
opcodeText = opcodeName . opcodeSpec

-- | Show 'PUSH' as the Haskell data constructor
instance {-# OVERLAPPING #-} Show Opcode where
  show (PUSH n) = "PUSH " <> show n
  show opcode = Text.unpack (Text.toUpper (opcodeText opcode))

instance {-# OVERLAPPABLE #-} Show a => Show (Opcode' a) where
  show opcode = show (concrete opcode) <> showParam opcode
    where
      showParam (JUMP a) = " " <> show a
      showParam (JUMPI a) = " " <> show a
      showParam (JUMPDEST a) = " " <> show a
      showParam _opcode = ""

-- | Calculate the size in bytes of an encoded opcode. The only 'Opcode'
-- that uses more than one byte is 'PUSH'. Sizes are trivially determined
-- for only 'Opcode' with unlabelled jumps, since we cannot know e.g. where
-- the label of a 'LabelledOpcode' points to before code generation has
-- completed.
opcodeSize :: Num i => Opcode -> i
opcodeSize (PUSH n) = List.genericLength . uncurry (:) $ push' n
opcodeSize _opcode = 1

-- | Convert a @['Opcode']@ to a string of ASCII hexadecimals.
toHex :: IsString s => [Opcode] -> s
toHex = fromString . List.concatMap (printf "%02x") . List.concatMap toBytes

-- | Convert a @['Opcode']@ to bytecode.
pack :: [Opcode] -> ByteString
pack = BS.pack . List.concatMap toBytes

-- | Convert an @'Opcode'@ to a @['Word8']@.
--
-- To convert many 'Opcode's to bytecode, use 'pack'.
toBytes :: Opcode -> [Word8]
toBytes (PUSH n) = uncurry (:) (push' n)
toBytes opcode = [ opcodeEncoding (opcodeSpec opcode) ]

-- | Convert the constant argument of a 'PUSH' to the opcode encoding
-- (0x60--0x7f) and its constant split into 'Word8' segments.
push' :: Word256 -> (Word8, [Word8])
push' i | i < 256 = (0x60, [fromIntegral i])
push' i = (opcode + 1, arg <> [fromIntegral i])
  where (opcode, arg) = push' (i `shift` (-8))

-- | Pattern synonyms for 'DUP', 'SWAP' and 'LOG'.
--
-- Use 'DUP1' instead of @'DUP' 'Ord16_1'@, etc.
pattern DUP1 = DUP Ord16_1
pattern DUP2 = DUP Ord16_2
pattern DUP3 = DUP Ord16_3
pattern DUP4 = DUP Ord16_4
pattern DUP5 = DUP Ord16_5
pattern DUP6 = DUP Ord16_6
pattern DUP7 = DUP Ord16_7
pattern DUP8 = DUP Ord16_8
pattern DUP9 = DUP Ord16_9
pattern DUP10 = DUP Ord16_10
pattern DUP11 = DUP Ord16_11
pattern DUP12 = DUP Ord16_12
pattern DUP13 = DUP Ord16_13
pattern DUP14 = DUP Ord16_14
pattern DUP15 = DUP Ord16_15
pattern DUP16 = DUP Ord16_16

pattern SWAP1 = SWAP Ord16_1
pattern SWAP2 = SWAP Ord16_2
pattern SWAP3 = SWAP Ord16_3
pattern SWAP4 = SWAP Ord16_4
pattern SWAP5 = SWAP Ord16_5
pattern SWAP6 = SWAP Ord16_6
pattern SWAP7 = SWAP Ord16_7
pattern SWAP8 = SWAP Ord16_8
pattern SWAP9 = SWAP Ord16_9
pattern SWAP10 = SWAP Ord16_10
pattern SWAP11 = SWAP Ord16_11
pattern SWAP12 = SWAP Ord16_12
pattern SWAP13 = SWAP Ord16_13
pattern SWAP14 = SWAP Ord16_14
pattern SWAP15 = SWAP Ord16_15
pattern SWAP16 = SWAP Ord16_16

pattern LOG0 = LOG Ord5_0
pattern LOG1 = LOG Ord5_1
pattern LOG2 = LOG Ord5_2
pattern LOG3 = LOG Ord5_3
pattern LOG4 = LOG Ord5_4
