module Parser.Smarts where

import Data.AST
import Parser.Atom
import Parser.Bond
import Parser.Ring

import Text.Parsec
import Text.Parsec.String (Parser)

recursiveSmarts :: Parser SmartsComponent
recursiveSmarts = many smartsExpr

branch :: Parser SmartsComponent
branch = do
    _ <- char '('
    exprs <- many smartsExpr
    _ <- char ')'
    return exprs

smartsExpr :: Parser SmartsExpr
smartsExpr = try (AtomExpr <$> atomicEnvironment recursiveSmarts)
          <|> try (BondExpr <$> bondEnvironment)
          <|> try (RingExpr <$> ringLabel)
          <|> try (Branch <$> branch)

smartsComponent :: Parser SmartsComponent
smartsComponent = many smartsExpr

smarts :: Parser Smarts
smarts = do
    first <- smartsComponent
    rest <- many (char '.' *> smartsComponent)
    return (first : rest)

smartsEof :: Parser Smarts
smartsEof = do
    components <- smarts
    eof
    return components

-- type Smarts = [SmartsComponent]
-- data ReactionSmarts = ReactionSmarts Smarts Smarts Smarts deriving (Show, Eq)
reactionSmartsEof :: Parser ReactionSmarts
reactionSmartsEof = do
    reactant <- smarts
    _ <- string ">"
    agent <- smarts
    _ <- string ">"
    product <- smarts
    eof
    return $ ReactionSmarts reactant agent product

parseSmarts :: String -> Either ParseError Smarts
parseSmarts = parse smartsEof ""

parseReactionSmarts :: String -> Either ParseError ReactionSmarts
parseReactionSmarts = parse reactionSmartsEof ""
