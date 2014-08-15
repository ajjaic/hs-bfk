import Bfparser

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Data.Word (Word8)
import Data.Array

import Control.Monad.IO.Class
type Size = Int

data Machine = Machine {
                memarray :: Array Int Word8,
                instrptr :: Int,
                memsize :: Size
                }

newMachine :: Size -> Machine
newMachine s = Machine (array (1, s) $ zip [1..] (repeat 0)) 1 s

eAtPtr :: Machine -> Word8
eAtPtr m = (memarray m) ! (instrptr m)

movePtr :: Machine -> (Int -> Int) -> Maybe Machine
movePtr m fn = if newptr > (memsize m)
                then Nothing
                else Just m {instrptr = newptr}
    where
     newptr = fn (instrptr m)

modifyByte :: Machine -> (Word8 -> Word8) -> Machine
modifyByte m fn = m {memarray = mem // [(ptr, fn $ eAtPtr m)]}
    where mem = memarray m
          ptr = instrptr m

setByteAtPtr :: Machine -> Word8 -> Machine
setByteAtPtr m w = m `modifyByte` (*0) `modifyByte` (+w)

--

type MachineState a = MaybeT (State Machine) a

executeProgram :: Program -> MachineState (Maybe Word8)
executeProgram (Program cmds) = executeProgram' cmds where
    executeProgram' [] = undefined
    executeProgram' (x:xs) = do
        case x of
            IncDataPtr   -> incDataPtr
            DecDataPtr   -> decDataPtr
            IncByteAtPtr -> incByteAtPtr
            DecByteAtPtr -> decByteAtPtr
            OpByteAtPtr  -> undefined
            IpByteAtPtr  -> undefined
            Loop cmds    -> undefined
        executeProgram' xs

incDataPtr :: MachineState (Maybe ())
incDataPtr = do
    m <- lift get
    maybe (return Nothing) updatestate $ movePtr m (+1)
    where
     updatestate m' = do
         lift $ put m'
         return $ Just ()

decDataPtr :: MachineState (Maybe ())
decDataPtr = do
    m <- lift get
    maybe (return Nothing) updatestate $ movePtr m (subtract 1)
    where
     updatestate m' = do
         lift $ put m'
         return $ Just ()

incByteAtPtr :: MachineState (Maybe ())
incByteAtPtr = do
    m <- lift get
    let nm = modifyByte m (+1)
    lift $ put nm
    return $ Just ()

decByteAtPtr :: MachineState (Maybe ())
decByteAtPtr = do
    m <- lift get
    let nm = modifyByte m (subtract 1)
    lift $ put nm
    return $ Just ()

opByteAtPtr :: MachineState (Maybe Word8)
opByteAtPtr = do
    m <- lift get
    let e = eAtPtr m
    return $ Just e

ipByteAtPtr :: Word8 -> MachineState (Maybe ())
ipByteAtPtr w = do
    m <- lift get
    let nm = setByteAtPtr m w
    lift $ put nm
    return $ Just ()

exLoop :: [Command] -> MachineState (Maybe ())
exLoop cmds = undefined

