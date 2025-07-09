module Semantic.Graph where

import Data.Graph.Inductive.Graph
import Data.AST
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Error (newErrorMessage, Message(..))
import Text.Parsec.Pos (initialPos)
import Parser.Smarts
import Data.Either


type Atom = LNode AtomicEnvironment
type Bond = LEdge BondEnvironment
type Ring = (RingLabel, Atom)

data FirstPassDatum
    = AtomD Atom
    | BondD Bond
    | RingD Ring
    deriving (Show, Eq)


newtype SemanticError = SemanticError String deriving (Show, Eq)

-- If both match the above, it's aromatic
deriveBondType :: AtomicEnvironment -> AtomicEnvironment -> BondType
deriveBondType (AtomicPrimitive (Element (AromaticElementSymbol _))) (AtomicPrimitive (Element (AromaticElementSymbol _))) = Aromatic
deriveBondType (AtomicPrimitive (Element (AnyElement AnyAromatic))) (AtomicPrimitive (Element (AromaticElementSymbol _))) = Aromatic
deriveBondType (AtomicPrimitive (Element (AromaticElementSymbol _))) (AtomicPrimitive (Element (AnyElement AnyAromatic))) = Aromatic
deriveBondType (AtomicPrimitive (Element (AnyElement AnyAromatic))) (AtomicPrimitive (Element (AnyElement AnyAromatic))) = Aromatic
deriveBondType _ _ = Single

-- Return both the resulting data and the next available index
firstPass :: Int -> Maybe Atom -> [SmartsExpr] -> ([FirstPassDatum], Int)
-- Root of AST
firstPass index Nothing ((AtomExpr atomExpr):rest) =
    let atom = (index, atomExpr)
        (restData, nextIndex) = firstPass (index+1) (Just atom) rest
    in (AtomD atom : restData, nextIndex)
firstPass index Nothing _ = error "Invalid input"

-- Tree branches
firstPass index (Just prevAtom) ((AtomExpr atomExpr):rest) =
    let atom = (index, atomExpr)
        bond = (fst prevAtom, index, BondType $ deriveBondType (snd prevAtom) atomExpr)
        (restData, nextIndex) = firstPass (index+1) (Just atom) rest
    in (BondD bond : AtomD atom : restData, nextIndex)

firstPass index (Just prevAtom) ((BondExpr bondExpr):(AtomExpr atomExpr):rest) =
    let atom = (index, atomExpr)
        bond = (fst prevAtom, index, bondExpr)
        (restData, nextIndex) = firstPass (index+1) (Just atom) rest
    in (BondD bond : AtomD atom : restData, nextIndex)

-- Handle rings
firstPass index (Just prevAtom) ((RingExpr ringLabel):rest) =
    let ring = RingD (ringLabel, prevAtom)
        (restData, nextIndex) = firstPass index (Just prevAtom) rest
    in (ring : restData, nextIndex)

-- Handle branches
firstPass index (Just prevAtom) ((Branch branch):rest) =
    let (innerData, branchNextIndex) = firstPass index (Just prevAtom) branch
        -- Use branchNextIndex when processing the rest
        (restData, finalIndex) = firstPass branchNextIndex (Just prevAtom) rest
    in (innerData ++ restData, finalIndex)

-- Handle leaves
firstPass index (Just prevAtom) []  = ([], index)
firstPass _ _ _ = error "Invalid input"


-- Get bonds from rings by connecting atoms with the same ring label
getBondsFromRings :: [FirstPassDatum] -> [Bond]
getBondsFromRings firstPassData =
    let rings = [(label, atom) | RingD (label, atom) <- firstPassData]
        ringsByLabel = groupByLabel rings
    in concatMap makeBondsForRingGroup ringsByLabel
  where
    groupByLabel rings =
        Map.toList $ foldr (\(label, atom) -> Map.insertWith (++) label [atom]) Map.empty rings

    makeBondsForRingGroup (_, [_]) = [] -- Need at least 2 atoms to form a ring bond
    makeBondsForRingGroup (_, atoms) =
        [(fst a1, fst a2, BondType $ deriveBondType (snd a1) (snd a2)) |
         a1 <- atoms,
         a2 <- atoms,
         fst a1 < fst a2] -- Ensure each pair is only counted once

secondPass :: Graph gr => [FirstPassDatum] -> gr AtomicEnvironment BondEnvironment
secondPass firstPassData = mkGraph nodes edges
  where
    -- Extract nodes and edges from the first pass data
    nodes = [(i, env) | AtomD (i, env) <- firstPassData]
    explicitBonds = [(u, v, bondEnv) | BondD (u, v, bondEnv) <- firstPassData]
    ringBonds = getBondsFromRings firstPassData
    edges = explicitBonds ++ ringBonds

-- Convert a SMARTS expression to a graph representation
semanticGraph :: Graph gr => [SmartsExpr] -> Either SemanticError (gr AtomicEnvironment BondEnvironment)
semanticGraph smartsExprs =
    case smartsExprs of
        [] -> Left (SemanticError "Empty SMARTS expression")
        exprs -> Right $ secondPass $ fst $ firstPass 0 Nothing exprs

-- Parse a SMARTS string into a list of graph representations
parseToGraphs :: Graph gr => String -> Either ParseError [gr AtomicEnvironment BondEnvironment]
parseToGraphs input = do
    components <- parse smartsEof "" input
    -- Convert each component to a graph
    return $ rights $ map (either (Left . toParseError) Right . semanticGraph) components
  where
    -- Convert semantic error to parse error for consistency
    toParseError (SemanticError msg) = 
        newErrorMessage (Message msg) (initialPos "")
