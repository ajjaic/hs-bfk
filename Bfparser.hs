{-# LANGUAGE OverloadedStrings #-}

module Bfparser (
    Program (..)
  , Command (..)
  , parseProgram
) where

import Data.Attoparsec.Text (Parser, endOfInput, char, many')
import Control.Applicative ((<|>), (<*), (*>), (<$>))
{-import Data.Text-}

{-m1 = "[+++..>>>>--[,,..++--]++--]"-}
{-m2 = "+++..>>>>--,,..++--++--"-}

{-s1 = parseTest parseProgram "<>b><"-}
{-s2 = parse parseProgram "<>b><"-}
{-s3 = parseOnly parseProgram m1-}
{-s4 = parseOnly parseProgram m2-}

newtype Program = Program [Command] deriving (Show)

data Command = IncDataPtr
             | DecDataPtr
             | IncByteAtPtr
             | DecByteAtPtr
             | OpByteAtPtr
             | IpByteAtPtr
             | Loop [Command] deriving (Show, Eq)

parseCommands :: Parser Command
parseCommands = parseIncDataPtr
            <|> parseDecDataPtr
            <|> parseIncByteAtPtr
            <|> parseDecByteAtPtr
            <|> parseOpByteAtPtr
            <|> parseIpByteAtPtr
            <|> parseLoop

parseIncDataPtr :: Parser Command
parseIncDataPtr = const IncDataPtr <$> char '>'

parseDecDataPtr :: Parser Command
parseDecDataPtr = const DecDataPtr <$> char '<'

parseIncByteAtPtr :: Parser Command
parseIncByteAtPtr = const IncByteAtPtr <$> char '+'

parseDecByteAtPtr :: Parser Command
parseDecByteAtPtr = const DecByteAtPtr <$> char '-'

parseOpByteAtPtr :: Parser Command
parseOpByteAtPtr = const OpByteAtPtr <$> char '.'

parseIpByteAtPtr :: Parser Command
parseIpByteAtPtr = const IpByteAtPtr <$> char ','

parseLoop :: Parser Command
parseLoop = Loop <$> (char '[' *> many' parseCommands <* char ']')

parseProgram :: Parser Program
parseProgram = Program <$> many' parseCommands <* endOfInput


