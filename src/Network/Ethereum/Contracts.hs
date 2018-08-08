
module Network.Ethereum.Contracts where

import           Language.Evm
import           Language.Evm.Types

import           Prelude hiding (return)


initEvmContract :: EvmAsm -> EvmCode
initEvmContract body =
  let bodyLen = length $ codegen body
      initLen = 14
      wrapped = codegen $ do
        -- <initLen> (count the bytes)
        push2 $ fromIntegral bodyLen  -- 3
        dup1                          -- 1
        push2 $ fromIntegral initLen  -- 3
        push1 0                       -- 2
        codecopy                      -- 1
        _jump "_skip"                 -- 4
        -- </initLen>
        body
        _dest "_skip"
        push1 0
        return
        -- Neccesary to splice generated code in so that jump destinations are correct for revert
  in  take (initLen*2) wrapped ++
      codegen body ++
      drop (initLen*2+bodyLen) wrapped

contractProxyCode :: Integer -> EvmCode
contractProxyCode = codegen . contractProxy

contractProxy :: Integer -> EvmAsm
contractProxy addr = do
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
