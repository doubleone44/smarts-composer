module Data.AST where

import Data.Functor.Product (Product)
import System.Console.GetOpt (OptDescr(Option))

data Or a = Or a a
  deriving (Show, Eq)

data And a = And a a
  deriving (Show, Eq)

newtype Not a = Not a
  deriving (Show, Eq)

data ElementSymbol = B | C | N | O | P | S | F | Cl | Br | I | H
  deriving (Show, Eq)

data AromaticElementSymbol = AromaticB | AromaticC | AromaticN | AromaticO | AromaticS
  deriving (Show, Eq)

data AnyElement = AnyAliphatic | AnyAromatic | Any
  deriving (Show, Eq)

data Element
  = ElementSymbol ElementSymbol
  | AromaticElementSymbol AromaticElementSymbol
  | AtomicNumber Int
  | AnyElement AnyElement
  deriving (Show, Eq)

data AtomicPrimitive 
  = Element Element
  | Charge Int                -- Covers both +<n> and -<n>
  | Isotope Int               -- <n> atomic mass
  | Recursive [SmartsExpr]    
  | Degree Int                -- D<n>: <n> explicit connections
  | TotalHCount Int           -- H<n>: <n> attached hydrogens
  | ImplicitHCount Int        -- h<n>: <n> implicit hydrogens
  | RingMembership Int        -- R<n>: in <n> SSSR rings
  | RingSize Int              -- r<n>: smallest SSSR ring of size <n>
  | Valence Int               -- v<n>: total bond order <n>
  | Connectivity Int          -- X<n>: <n> total connections
  | RingConnectivity Int      -- x<n>: <n> total ring connections
  | ChiralityAnticlockwise    -- @: anticlockwise chirality  
  | ChiralityClockwise        -- @@: clockwise chirality
  | ChiralityClass Char Int   -- @<c><n>: chiral class <c>, chirality <n>
  | ChiralityClassUnspec Char Int  -- @<c><n>?: chiral or unspecified
  deriving (Show, Eq)

data AtomicEnvironment 
  = AtomicPrimitive AtomicPrimitive
  | AtomOr (Or AtomicEnvironment)
  | AtomAnd (And AtomicEnvironment)
  | AtomNot (Not AtomicEnvironment)
  deriving (Show, Eq)


data BondType 
  = Single 
  | Double 
  | Triple 
  | Aromatic 
  | AnyBond
  | RingBond
  | DirectionalUp
  | DirectionalDown
  | DirectionalUpOrUnspecified
  | DirectionalDownOrUnspecified
  deriving (Show, Eq)


data BondEnvironment 
  = BondType BondType
  | BondOr (Or BondEnvironment)
  | BondAnd (And BondEnvironment)
  | BondNot (Not BondEnvironment)
  deriving (Show, Eq)

type RingLabel = Int

data SmartsExpr
  = AtomExpr AtomicEnvironment
  | BondExpr BondEnvironment
  | RingExpr RingLabel
  | Branch SmartsComponent 
  deriving (Show, Eq)

type SmartsComponent = [SmartsExpr]

type Smarts = [SmartsComponent]

data ReactionSmarts = ReactionSmarts Smarts Smarts Smarts deriving (Show, Eq)