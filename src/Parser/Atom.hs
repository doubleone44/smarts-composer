module Parser.Atom where

import Data.AST
import Parser.Logical (buildLogicalExpr)
import Text.Parsec
import Text.Parsec.String (Parser)

elementSymbol :: Parser ElementSymbol
elementSymbol =
  choice
    [ string "B" >> return B,
      string "C" >> return C,
      string "N" >> return N,
      string "O" >> return O,
      string "P" >> return P,
      string "S" >> return S,
      string "F" >> return F,
      string "Cl" >> return Cl,
      string "Br" >> return Br,
      string "I" >> return I
    ]

aromaticElementSymbol :: Parser AromaticElementSymbol
aromaticElementSymbol =
  choice
    [ string "b" >> return AromaticB,
      string "c" >> return AromaticC,
      string "n" >> return AromaticN,
      string "o" >> return AromaticO,
      string "s" >> return AromaticS
    ]

atomicNumber :: Parser Int
atomicNumber = do
  _ <- char '#'
  n <- many1 digit
  return (read n :: Int)

anyElement :: Parser AnyElement
anyElement =
  choice
    [ string "A" >> return AnyAliphatic,
      string "a" >> return AnyAromatic,
      char '*' >> return Any
    ]

element :: Parser Element
element =
  choice
    [ ElementSymbol <$> elementSymbol,
      AromaticElementSymbol <$> aromaticElementSymbol,
      AnyElement <$> anyElement,
      AtomicNumber <$> atomicNumber
    ]

-- New parsers for all atomic primitives

-- Parser for charge: +n or -n
charge :: Parser AtomicPrimitive
charge = do
  sign <- char '+' <|> char '-'
  n <- option 1 (read <$> many1 digit)
  return $ Charge (if sign == '+' then n else (-n))

-- Parser for isotope: n before element
isotope :: Parser AtomicPrimitive
isotope = do
  n <- many1 digit
  notFollowedBy (char 'D' <|> char 'H' <|> char 'h' <|> char 'R' <|> char 'r' <|> char 'v' <|> char 'X' <|> char 'x')
  return $ Isotope (read n)

-- Parser for degree: Dn
degree :: Parser AtomicPrimitive
degree = do
  _ <- char 'D'
  n <- option 1 (read <$> many1 digit)
  return $ Degree n

-- Parser for total hydrogen count: Hn
totalHCount :: Parser AtomicPrimitive
totalHCount = do
  _ <- char 'H'
  n <- option 1 (read <$> many1 digit)
  return $ TotalHCount n

-- Parser for implicit hydrogen count: hn
implicitHCount :: Parser AtomicPrimitive
implicitHCount = do
  _ <- char 'h'
  n <- option 1 (read <$> many1 digit)
  return $ ImplicitHCount n

-- Parser for ring membership: Rn
ringMembership :: Parser AtomicPrimitive
ringMembership = do
  _ <- char 'R'
  n <- option 0 (read <$> many1 digit)
  return $ RingMembership n

-- Parser for ring size: rn
ringSize :: Parser AtomicPrimitive
ringSize = do
  _ <- char 'r'
  n <- read <$> many1 digit
  return $ RingSize n

-- Parser for valence: vn
valence :: Parser AtomicPrimitive
valence = do
  _ <- char 'v'
  n <- read <$> many1 digit
  return $ Valence n

-- Parser for connectivity: Xn
connectivity :: Parser AtomicPrimitive
connectivity = do
  _ <- char 'X'
  n <- option 1 (read <$> many1 digit)
  return $ Connectivity n

-- Parser for ring connectivity: xn
ringConnectivity :: Parser AtomicPrimitive
ringConnectivity = do
  _ <- char 'x'
  n <- read <$> many1 digit
  return $ RingConnectivity n

-- Parser for chirality
chirality :: Parser AtomicPrimitive
chirality = choice
  [ string "@@" >> return ChiralityClockwise
  , char '@' >> return ChiralityAnticlockwise
  ]

-- Parser for chirality class: @CHn where CH is a character and n is a digit
chiralityClass :: Parser AtomicPrimitive
chiralityClass = do
  _ <- char '@'
  ch <- anyChar
  n <- read <$> many1 digit
  option (ChiralityClass ch n) (char '?' >> return (ChiralityClassUnspec ch n))

-- Parametrize the recursivePattern function to accept a parser function
recursivePattern :: Parser [SmartsExpr] -> Parser [SmartsExpr]
recursivePattern smartsParser = do
    _ <- char '$'
    between (char '(') (char ')') smartsParser

elementalPrimitive :: Parser AtomicPrimitive
elementalPrimitive = Element <$> element

atomicPrimitive :: Parser [SmartsExpr] -> Parser AtomicPrimitive
atomicPrimitive smartsParser = choice
  [ try elementalPrimitive
  , try charge
  , try isotope
  , try degree
  , try totalHCount
  , try implicitHCount
  , try ringMembership
  , try ringSize
  , try valence
  , try connectivity
  , try ringConnectivity
  , try chiralityClass
  , try chirality
  , Recursive <$> recursivePattern smartsParser
  ]

atomicEnvironment :: Parser [SmartsExpr] -> Parser AtomicEnvironment
atomicEnvironment smartsParser =
  choice
    [ between (char '[') (char ']') logicalAtomicEnvironment
    , AtomicPrimitive <$> atomicPrimitive smartsParser
    ]
  where
    logicalAtomicEnvironment =
      buildLogicalExpr
        AtomicPrimitive
        AtomOr
        AtomAnd
        AtomNot
        (atomicPrimitive smartsParser)