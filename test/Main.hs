module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.AST
import Parser.Smarts
import Semantic.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Text.Parsec
import Data.Either

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, graphTests]

parserTests :: TestTree
parserTests = testGroup "Parser tests"
  [ testCase "Simple atom" $
      parseSmarts "C" @?= Right [[AtomExpr (AtomicPrimitive (Element (ElementSymbol C)))]]
      
  , testCase "Atom with charge" $
      parseSmarts "[C+]" @?= Right [[AtomExpr (AtomOr (Or (AtomicPrimitive (Element (ElementSymbol C))) (AtomicPrimitive (Charge 1))))]]
      
  , testCase "Atom with implicit H count" $
      parseSmarts "[Ch1]" @?= Right [[AtomExpr (AtomOr (Or (AtomicPrimitive (Element (ElementSymbol C))) (AtomicPrimitive (ImplicitHCount 1))))]]
      
  , testCase "Carbon-carbon bond" $
      parseSmarts "CC" @?= Right [[
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C))),
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C)))
      ]]
      
  , testCase "Carbon-carbon with explicit bond" $
      parseSmarts "C-C" @?= Right [[
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C))),
        BondExpr (BondType Single),
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C)))
      ]]
      
  , testCase "Carbon ring" $
      parseSmarts "C1CCC1" @?= Right [[
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C))),
        RingExpr 1,
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C))),
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C))),
        AtomExpr (AtomicPrimitive (Element (ElementSymbol C))),
        RingExpr 1
      ]]
  ]

graphTests :: TestTree
graphTests = testGroup "Graph construction tests"
  [ testCase "Simple atom" $
      isRight (parseToGraphs "C" :: Either ParseError [Gr AtomicEnvironment BondEnvironment]) @?= True
      
  , testCase "Carbon chain" $
      isRight (parseToGraphs "CCC" :: Either ParseError [Gr AtomicEnvironment BondEnvironment]) @?= True
      
  , testCase "Carbon ring" $
      isRight (parseToGraphs "C1CCC1" :: Either ParseError [Gr AtomicEnvironment BondEnvironment]) @?= True
      
  , testCase "Multiple components" $
      case parseToGraphs "C.N.O" :: Either ParseError [Gr AtomicEnvironment BondEnvironment] of
        Right graphs -> length graphs @?= 3
        Left _ -> assertFailure "Failed to parse multiple components"
  ]