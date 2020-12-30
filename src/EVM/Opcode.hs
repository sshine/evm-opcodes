{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module: EVM.Opcode
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exposes the 'Opcode' type for expressing Ethereum VM opcodes
-- as extracted from the Ethereum Yellow Paper with amendments from various
-- EIPs. The Yellow Paper is available at:
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

import EVM.Opcode.Internal
