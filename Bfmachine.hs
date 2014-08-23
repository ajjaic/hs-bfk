{-module Bfmachine (-}
      {-incDataPtr-}
    {-, decDataPtr-}
    {-, incByteAtPtr-}
    {-, decByteAtPtr-}
    {-, opByteAtPtr-}
    {-, ipByteAtPtr-}
    {-, exLoop-}
    {-, newMachine-}
    {-, prop_conformstart-}
{-) where-}

module Bfmachine where

import Bfparser

import Control.Monad.Trans.State.Lazy (StateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Class (lift)
import Data.Word (Word8)
import Data.Array (Array, array, bounds, elems, (!), (//))
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import Test.QuickCheck

type Ptr     = Int
type Size    = Int
data Machine = Machine {
                memarray :: Array Ptr Word8,
                instrptr :: Ptr,
                memsize  :: Size
                } deriving (Show)

newMachine :: Size -> Machine
newMachine s = Machine (array (1, s) $ zip [1..s] (repeat 0)) 1 s

eAtPtr :: Machine -> Word8
eAtPtr m = (memarray m) ! (instrptr m)

movePtr :: Machine -> (Ptr -> Ptr) -> Maybe Machine
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


exCommand :: Command -> MachineStateIO ()
exCommand IncDataPtr   = incDataPtr
exCommand DecDataPtr   = decDataPtr
exCommand IncByteAtPtr = incByteAtPtr
exCommand DecByteAtPtr = decByteAtPtr
exCommand OpByteAtPtr  = opByteAtPtr
exCommand IpByteAtPtr  = ipByteAtPtr
exCommand (Loop cmds)  = exLoop cmds

executeProgram :: Program -> MachineStateIO ()
executeProgram (Program cmds) = sequence_ $ map exCommand cmds

incDataPtr :: MachineStateIO ()
incDataPtr = do
    m <- lift get
    maybe (MaybeT $ return Nothing) updatestate $ movePtr m (+1)
    where
     updatestate m' = do
        lift $ put m'
        return ()

decDataPtr :: MachineStateIO ()
decDataPtr = do
    m <- lift get
    maybe (MaybeT $ return Nothing) updatestate $ movePtr m (subtract 1)
    where
     updatestate m' = do
         lift $ put m'
         return ()

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
    lift $ liftIO $ putStrLn (show e)
    return ()

ipByteAtPtr :: MachineStateIO ()
ipByteAtPtr = do
    m <- lift get
    b <- lift $ liftIO getbyte
    maybe (MaybeT $ return Nothing) (insertbyte m) b
    where
     insertbyte m b = do
        let nm = setByteAtPtr m b
        lift $ put nm
        return ()
     getbyte = do
        s <- getLine
        let byte_s = reads s :: [(Word8, String)]
        case byte_s of
            [(a,"")] -> return $ Just a
            _ -> return Nothing

exLoop :: [Command] -> MachineStateIO ()
exLoop cmds = do
    m <- lift get
    let e = eAtPtr m
    if e == 0
        then return ()
        else sequence_ (map exCommand cmds) >> exLoop cmds

--Property based tests

instance Arbitrary Machine where
    arbitrary = newMachine <$> (choose (100, 200))

prop_conformstart :: Property
prop_conformstart = forAll (arbitrary :: Gen Machine) conformsize
    where
     conformsize (Machine arr ptr size) = size >= 100
                                            && size <= 200
                                            && ptr == 1
                                            && (bounds arr) == (1, size)
                                            && (null $ filter (/=0) (elems arr))

fmain = quickCheck prop_conformstart
