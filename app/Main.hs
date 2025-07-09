module Main where

import Text.Parsec
import Data.AST
import Parser.Smarts
import Semantic.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)


main :: IO ()
main = do
    putStrLn "Enter a SMARTS string to parse:"
    input <- getLine
    case parseToGraphs input :: Either ParseError [Gr AtomicEnvironment BondEnvironment] of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right result -> putStrLn $ "Parsed successfully: " ++ show result

