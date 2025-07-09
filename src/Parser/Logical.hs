module Parser.Logical where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.AST

buildLogicalExpr ::
  (p -> a)
  -> (Or a -> a)
  -> (And a -> a)
  -> (Not a -> a)
  -> Parser p
  -> Parser a
buildLogicalExpr primCtor orCtor andCtor notCtor primParser = lowAndExpr
  where
    -- Term is the parsed primitive
    term = primCtor <$> primParser 

    -- Not (!) has highest precedence
    notExpr = (notCtor . Not <$> (char '!' *> term)) <|> term

    -- HighAnd (&) has second highest precedence
    highAndExpr = chainl1 notExpr highAndOp
    highAndOp = (\x y -> andCtor (And x y)) <$ (char '&' <|> return '&')

    -- Or (,) has third highest precedence
    orExpr = chainl1 highAndExpr orOp  
    orOp = (\x y -> orCtor (Or x y)) <$ char ','

    -- LowAnd (;) has lowest precedence
    lowAndExpr = chainl1 orExpr lowAndOp
    lowAndOp = (\x y -> andCtor (And x y)) <$ char ';'
