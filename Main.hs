{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text --(Parser, char, choice, parseTest, (<?>), many')
import Control.Applicative --((<$>), pure)
import Data.Text

m :: Text
m = "[+++..>>>>--[,,..++--]++--]"

newtype Program = Program [Command] deriving (Show)

data Command = Noop
             | IncDataPtr
             | DecDataPtr
             | IncByte
             | DecByte
             | OutputByte
             | InputByte
             | Loop [Command] deriving (Show, Eq)

parseIncDataPtr :: Parser Command
parseIncDataPtr = const IncDataPtr <$> char '>'

parseDecDataPtr :: Parser Command
parseDecDataPtr = const DecDataPtr <$> char '<'

parseProgram :: Parser Program
parseProgram = Program <$> (many' $ choice [parseIncDataPtr, parseDecDataPtr])


testing = parseTest parseProgram "<<|"
testing' = parseOnly parseProgram "<<>|><"
