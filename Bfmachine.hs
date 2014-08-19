module Bfmachine (
    incDataPtr
  , MachineStateIO
) where

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
type MachineStateIO a = MaybeT (StateT Machine IO) a

{-executeProgram :: Program -> MachineStateIO (Maybe ())-}
{-executeProgram (Program cmds) = executeProgram' cmds where-}
    {-executeProgram' [] = return (Just ())-}
    {-executeProgram' (x:xs) = do-}
        {-case x of-}
            {-IncDataPtr   -> incDataPtr-}
            {-DecDataPtr   -> decDataPtr-}
            {-IncByteAtPtr -> incByteAtPtr-}
            {-DecByteAtPtr -> decByteAtPtr-}
            {-OpByteAtPtr  -> opByteAtPtr-}
            {-IpByteAtPtr  -> ipByteAtPtr-}
            {-Loop c       -> exLoop-}
        {-executeProgram' xs-}

exCommand :: Command -> MachineStateIO (Maybe ())
exCommand = undefined

an :: MachineStateIO ()
an = MaybeT (return $ (Just ()))

bin :: MachineStateIO ()
bin = do
    m <- lift get
    maybe (MaybeT $ return Nothing) updatestate $ movePtr m (+1)
    where
     updatestate m' = do
        lift $ put m'
        return ()

type MachineM a = StateT Machine IO (Maybe a)

{-newI :: StateT Machine IO-}

incDataPtr :: MachineStateIO (Maybe ())
incDataPtr = do
    m <- lift get
    maybe (return Nothing) updatestate $ movePtr m (+1)
    where
     updatestate m' = do
         lift $ put m'
         return $ Just ()

decDataPtr :: MachineStateIO (Maybe ())
decDataPtr = do
    m <- lift get
    maybe (return Nothing) updatestate $ movePtr m (subtract 1)
    where
     updatestate m' = do
         lift $ put m'
         return $ Just ()

incByteAtPtr :: MachineStateIO ()
incByteAtPtr = do
    m <- lift get
    let nm = modifyByte m (+1)
    lift $ put nm
    return ()

decByteAtPtr :: MachineStateIO ()
decByteAtPtr = do
    m <- lift get
    let nm = modifyByte m (subtract 1)
    lift $ put nm
    return ()

opByteAtPtr :: MachineStateIO ()
opByteAtPtr = do
    m <- lift get
    let e = eAtPtr m
    lift $ liftIO $ putStrLn $ show e
    return ()

{-ipByteAtPtr :: MachineStateIO ()-}
{-ipByteAtPtr = do-}
    {-m <- lift get-}
    {-b <- lift $ liftIO getbyte-}
    {-maybe (return Nothing) (insertbyte m) b-}
    {-where-}
     {-insertbyte m b = do-}
        {-let nm = setByteAtPtr m b-}
        {-lift $ put nm-}
        {-return ()-}
     {-getbyte = do-}
        {-s <- getLine-}
        {-let byte_s = reads s :: [(Word8, String)]-}
        {-case byte_s of-}
            {-[(a,"")] -> return $ Just a-}
            {-_ -> return Nothing-}

{-exLoop :: [Command] -> MachineStateIO ()-}
{-exLoop cmds = do-}
    {-m <- lift get-}
    {-let e = eAtPtr m-}
    {-if e == 0 then return-}
