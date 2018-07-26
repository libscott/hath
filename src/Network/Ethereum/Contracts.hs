
module Network.Ethereum.Contracts where

import           Language.Evm

import           Prelude hiding (return)



delegatecallInitCode :: Integer -> EvmAsm
delegatecallInitCode addr = do
  let sublen = fromIntegral $ length $ codegen $ delegatecallCode addr
  push2 $ sublen
  dup1
  push2 14
  push1 0
  codecopy
  push2 (14 + sublen)
  jump
  delegatecallCode addr
  jumpdest
  push1 0
  return


delegatecallCode :: Integer -> EvmAsm
delegatecallCode addr = do
  -- First thing, copy call data to memory
  calldatasize
  push1 0
  push1 0
  calldatacopy

  -- Call native contract
  push1 0       -- write buffer size
  push1 0       -- write to
  calldatasize  -- input len
  push1 0       -- read from
  push20 addr
  gas
  delegatecall  -- This pushes exit code onto the stack

  -- Copy the output to memory
  returndatasize
  push1 0
  dup1
  returndatacopy

  -- Conditionally jump to revert
  push1 0
  eq
  _jumpi "_revert"

  -- Return
  returndatasize
  push1 0
  return

  -- Revert
  _dest "_revert"
  returndatasize
  push1 0
  revert
