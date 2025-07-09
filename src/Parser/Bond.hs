module Parser.Bond where

import Data.AST
import Parser.Logical (buildLogicalExpr)
import Text.Parsec
import Text.Parsec.String (Parser)


bondType :: Parser BondType
bondType = choice
  [ char '-' >> return Single
  , char '=' >> return Double
  , char '#' >> return Triple
  , char ':' >> return Aromatic
  , char '~' >> return AnyBond
  , char '@' >> return RingBond
  , char '/' >> return DirectionalUp
  , char '\\' >> return DirectionalDown
  , try (string "/?") >> return DirectionalUpOrUnspecified
  , try (string "\\?") >> return DirectionalDownOrUnspecified
  ]

bondEnvironment :: Parser BondEnvironment
bondEnvironment = buildLogicalExpr
    BondType BondOr BondAnd BondNot bondType
