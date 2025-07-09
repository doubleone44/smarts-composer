module Parser.Ring where

import Data.AST
import Text.Parsec
import Text.Parsec.String (Parser)


-- Parsers single digits from 1-9, fails on 0.
ringNumber :: Parser Int
ringNumber = do
    n <- oneOf "123456789"
    return (read [n] :: Int)

-- Parses e.g. %11 to get the ring number 11.
ringPercentSign :: Parser Int
ringPercentSign = do 
    _ <- char '%'
    n <- many1 digit
    return (read n :: Int)


ringLabel :: Parser RingLabel
ringLabel = ringNumber <|> ringPercentSign
