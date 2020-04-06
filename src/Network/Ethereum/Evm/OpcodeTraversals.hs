{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Network.Ethereum.Evm.OpcodeTraversals
-- Copyright: 2018 Simon Shine
-- Maintainer: Simon Shine <shreddedglory@gmail.com>
-- License: MIT
--
-- This module exposes generic methods of traversing `AbstractOpcode`s.

module Network.Ethereum.Evm.OpcodeTraversals where

import Prelude hiding (LT, EQ, GT)
import Network.Ethereum.Evm.Opcode
import Control.Monad.Identity

data OpcodeMapperM m a b = OpcodeMapperM
  { mapOnJump     :: a -> m (AbstractOpcode b)
  , mapOnJumpi    :: a -> m (AbstractOpcode b)
  , mapOnJumpDest :: a -> m (AbstractOpcode b)
  , mapOnOther    :: AbstractOpcode a -> m (Maybe (AbstractOpcode b))
  }

type OpcodeMapper = OpcodeMapperM Identity

mapOpcode :: OpcodeMapper a b -> AbstractOpcode a -> AbstractOpcode b
mapOpcode mapper = runIdentity . mapOpcodeM mapper

mapOpcodeM :: forall m a b. Monad m => OpcodeMapperM m a b -> AbstractOpcode a -> m (AbstractOpcode b)
mapOpcodeM mapper opcode = case opcode of
  JUMP a     -> mapOnJump mapper a
  JUMPI a    -> mapOnJumpi mapper a
  JUMPDEST a -> mapOnJumpDest mapper a

  -- 0s: Stop and Arithmetic Operations
  STOP       -> mapOnOther' STOP STOP
  ADD        -> mapOnOther' ADD ADD
  MUL        -> mapOnOther' MUL MUL
  SUB        -> mapOnOther' SUB SUB
  DIV        -> mapOnOther' DIV DIV
  SDIV       -> mapOnOther' SDIV SDIV
  MOD        -> mapOnOther' MOD MOD
  SMOD       -> mapOnOther' SMOD SMOD
  ADDMOD     -> mapOnOther' ADDMOD ADDMOD
  MULMOD     -> mapOnOther' MULMOD MULMOD
  EXP        -> mapOnOther' EXP EXP
  SIGNEXTEND -> mapOnOther' SIGNEXTEND SIGNEXTEND

  -- 10s: Comparison & Bitwise Logic Operations
  LT      -> mapOnOther' LT LT
  GT      -> mapOnOther' GT GT
  SLT     -> mapOnOther' SLT SLT
  SGT     -> mapOnOther' SGT SGT
  EQ      -> mapOnOther' EQ EQ
  ISZERO  -> mapOnOther' ISZERO ISZERO
  AND     -> mapOnOther' AND AND
  OR      -> mapOnOther' OR OR
  XOR     -> mapOnOther' XOR XOR
  NOT     -> mapOnOther' NOT NOT
  BYTE    -> mapOnOther' BYTE BYTE

  -- 20s: SHA3
  SHA3 -> mapOnOther' SHA3 SHA3

  -- 30s: Environmental Information
  ADDRESS       -> mapOnOther' ADDRESS ADDRESS
  BALANCE       -> mapOnOther' BALANCE BALANCE
  ORIGIN        -> mapOnOther' ORIGIN ORIGIN
  CALLER        -> mapOnOther' CALLER CALLER
  CALLVALUE     -> mapOnOther' CALLVALUE CALLVALUE
  CALLDATALOAD  -> mapOnOther' CALLDATALOAD CALLDATALOAD
  CALLDATASIZE  -> mapOnOther' CALLDATASIZE CALLDATASIZE
  CALLDATACOPY  -> mapOnOther' CALLDATACOPY CALLDATACOPY
  CODESIZE      -> mapOnOther' CODESIZE CODESIZE
  CODECOPY      -> mapOnOther' CODECOPY CODECOPY
  GASPRICE      -> mapOnOther' GASPRICE GASPRICE
  EXTCODESIZE   -> mapOnOther' EXTCODESIZE EXTCODESIZE
  EXTCODECOPY   -> mapOnOther' EXTCODECOPY EXTCODECOPY

  -- 40s: Block Information
  BLOCKHASH   -> mapOnOther' BLOCKHASH BLOCKHASH
  COINBASE    -> mapOnOther' COINBASE COINBASE
  TIMESTAMP   -> mapOnOther' TIMESTAMP TIMESTAMP
  NUMBER      -> mapOnOther' NUMBER NUMBER
  DIFFICULTY  -> mapOnOther' DIFFICULTY DIFFICULTY
  GASLIMIT    -> mapOnOther' GASLIMIT GASLIMIT

  -- 50s: Stack, Memory, Storage and Flow Operations
  POP       -> mapOnOther' POP POP
  MLOAD     -> mapOnOther' MLOAD MLOAD
  MSTORE    -> mapOnOther' MSTORE MSTORE
  MSTORE8   -> mapOnOther' MSTORE8 MSTORE8
  SLOAD     -> mapOnOther' SLOAD SLOAD
  SSTORE    -> mapOnOther' SSTORE SSTORE
  PC        -> mapOnOther' PC PC
  MSIZE     -> mapOnOther' MSIZE MSIZE
  GAS       -> mapOnOther' GAS GAS

  -- 60s & 70s: Push Operations
  PUSH n    -> mapOnOther' (PUSH n) (PUSH n)

  -- 80s: Duplication Operations (DUP)
  DUP i     -> mapOnOther' (DUP i) (DUP i)

  -- 90s: Exchange operations (SWAP)
  SWAP i    -> mapOnOther' (SWAP i) (SWAP i)

  -- a0s: Logging Operations (LOG)
  LOG i     -> mapOnOther' (LOG i) (LOG i)

  -- f0s: System Operations
  CREATE       -> mapOnOther' CREATE CREATE
  CALL         -> mapOnOther' CALL CALL
  CALLCODE     -> mapOnOther' CALLCODE CALLCODE
  RETURN       -> mapOnOther' RETURN RETURN
  DELEGATECALL -> mapOnOther' DELEGATECALL DELEGATECALL
  SUICIDE      -> mapOnOther' SUICIDE SUICIDE
  where
    mapOnOther' :: Monad m => AbstractOpcode a -> AbstractOpcode b -> m (AbstractOpcode b)
    mapOnOther' opa opbDefault = do
      res <- mapOnOther mapper opa
      case res of
        Just opb -> return opb
        Nothing  -> return opbDefault

