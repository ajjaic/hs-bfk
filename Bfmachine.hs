import Bfparser

import Control.Monad.Trans.State.Strict
import Data.Word (Word8)

data Machine = Machine { array :: [Word8], instrptr :: Int }

type MachineState a = StateT Machine IO a

newMachine :: Machine
newMachine = Machine (Prelude.replicate 3000 0) 0

exIncDataPtr :: MachineState ()
exIncDataPtr = do
    machine <- get
    let ptr = instrptr machine
    put $ machine {instrptr = ptr+1}

exDecDataPtr :: MachineState ()
exDecDataPtr = do
    machine <- get
    let ptr = instrptr machine
    put $ machine {instrptr = ptr-1}

{-exIncByteAtPtr :: MachineState ()-}
{-exIncByteAtPtr = do-}
    {-machine <- get-}
    {-let memory = array machine-}
        {-ptr = instrptr machine-}
        {-newbyteval = (memory !! ptr) + 1-}
    {-put $ machine-}

exDecByteAtPtr :: MachineState ()
exDecByteAtPtr = undefined

exOpByteAtPtr :: MachineState ()
exOpByteAtPtr = undefined

exIpByteAtPtr :: MachineState ()
exIpByteAtPtr = undefined

exLoop :: [Command] -> MachineState ()
exLoop = undefined


executeProgram :: Program -> MachineState ()
executeProgram (Program cmds) = executeProgram' cmds where
    executeProgram' (x:xs) = do
        case x of
            IncDataPtr   -> exIncDataPtr
            DecDataPtr   -> exDecDataPtr
            IncByteAtPtr -> exIncByteAtPtr
            DecByteAtPtr -> exDecByteAtPtr
            OpByteAtPtr  -> exOpByteAtPtr
            IpByteAtPtr  -> exIpByteAtPtr
            Loop cmds    -> exLoop cmds
        executeProgram' xs




    {-execute IncDataPtr = undefined-}
    {-execute DecDataPtr = undefined-}
    {-execute IncByteAtPtr = undefined-}
    {-execute DecByteAtPtr = undefined-}
    {-execute OpByteAtPtr = undefined-}
    {-execute IpByteAtPtr = undefined-}
    {-execute (Loop cmds) = undefined-}

