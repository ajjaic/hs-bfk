{-# LANGUAGE OverloadedStrings #-}

-- Try using withStateT
--Convert field accesses to Lenses

import Bfmachine
import Bfparser

import Data.Attoparsec.Text
import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.IO.Class (liftIO)

hello = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
getinput = ",+."
add2numbers = ",>,[<+>-]<."

main :: IO ()
main = do
    (Right a) <- return $ parseOnly parseProgram add2numbers
    evalStateT (st_io a) (newMachine 100)
        where
            st_io initialst = do
                sf <- runMaybeT $ executeProgram initialst
                case sf of
                    Nothing -> liftIO $ putStrLn "Something is wrong"
                    Just _  -> return ()








