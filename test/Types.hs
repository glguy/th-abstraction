{-# Language CPP, FlexibleContexts, TypeFamilies, KindSignatures, TemplateHaskell, GADTs #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

#if MIN_VERSION_template_haskell(2,8,0)
{-# Language PolyKinds #-}
#endif

{-|
Module      : Types
Description : Test cases for the th-abstraction package
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module defined types used for testing features of @th-abstraction@
on various versions of GHC.

-}
module Types where

#if __GLASGOW_HASKELL__ >= 704
import GHC.Exts (Constraint)
#endif

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib (starK)

type Gadt1Int = Gadt1 Int

infixr 6 :**:
data Gadt1 (a :: *) where
  Gadtc1 :: Int          -> Gadt1Int
  Gadtc2 :: (a,a)        -> Gadt1 a
  (:**:) :: Bool -> Char -> Gadt1 ()     -- This is declared infix
  (:!!:) :: Char -> Bool -> Gadt1 Double -- This is not

data Adt1 (a :: *) (b :: *) = Adtc1 (a,b) | Bool `Adtc2` Int

data Gadtrec1 a where
  Gadtrecc1, Gadtrecc2 :: { gadtrec1a :: a, gadtrec1b :: b } -> Gadtrec1 (a,b)

data Equal :: * -> * -> * -> * where
  Equalc :: (Read a, Show a) => [a] -> Maybe a -> Equal a a a

data Showable :: * where
  Showable :: Show a => a -> Showable

data R = R1 { field1, field2 :: Int }

data Gadt2 :: * -> * -> * where
  Gadt2c1 :: Gadt2 a [a]
  Gadt2c2 :: Gadt2 [a] a
  Gadt2c3 :: Gadt2 [a] [a]

data VoidStoS (f :: * -> *)

data StrictDemo = StrictDemo Int !Int {-# UNPACK #-} !Int

#if MIN_VERSION_template_haskell(2,7,0)

-- Data families

data family DF (a :: *)
data instance DF (Maybe a) = DFMaybe Int [a]

# if MIN_VERSION_template_haskell(2,8,0)
data family DF1 (a :: k)
# else
data family DF1 (a :: *)
# endif
data instance DF1 b = DF1 b

data family Quoted (a :: *)

# if MIN_VERSION_template_haskell(2,8,0)
data family Poly (a :: k)
# else
data family Poly (a :: *)
# endif
data instance Poly a = MkPoly

data family GadtFam (a :: *) (b :: *)
data instance GadtFam c d where
  MkGadtFam1 :: x   -> y                     -> GadtFam y x
  (:&&:)     :: e   -> f                     -> GadtFam [e] f     -- This is declard infix
  (:^^:)     :: Int -> Int                   -> GadtFam Int Int   -- This is not
  (:#%:)     :: { famRec1, famRec2 :: Bool } -> GadtFam Bool Bool -- Nor is this
  MkGadtFam4 :: (Int ~ z) => z               -> GadtFam z z
  MkGadtFam5 :: (q ~ Char) => q              -> GadtFam Bool Bool
infixl 3 :&&:, :#%:

data family FamLocalDec1 a
data family FamLocalDec2 a b c
#endif

#if __GLASGOW_HASKELL__ >= 704
type Konst (a :: Constraint) (b :: Constraint) = a
type PredSyn1 a b = Konst (Show a) (Read b)
type PredSyn2 a b = Konst (PredSyn1 a b) (Show a)
type PredSyn3 c   = Int ~ c

data PredSynT =
    PredSyn1 Int Int => MkPredSynT1 Int
  | PredSyn2 Int Int => MkPredSynT2 Int
  | PredSyn3 Int     => MkPredSynT3 Int
#endif

-- We must define these here due to Template Haskell staging restrictions
justCI :: ConstructorInfo
justCI =
  ConstructorInfo
    { constructorName       = 'Just
    , constructorVars       = []
    , constructorContext    = []
    , constructorFields     = [VarT (mkName "a")]
    , constructorStrictness = [notStrictAnnot]
    , constructorVariant    = NormalConstructor
    }

gadtRecVanillaCI :: ConstructorInfo
gadtRecVanillaCI =
  ConstructorInfo
    { constructorName       = 'Gadtrecc1
    , constructorVars       = [v1K, v2K]
    , constructorContext    =
         [equalPred a (AppT (AppT (TupleT 2) (VarT v1)) (VarT v2))]
    , constructorFields     = [VarT v1, VarT v2]
    , constructorStrictness = [notStrictAnnot, notStrictAnnot]
    , constructorVariant    = RecordConstructor ['gadtrec1a, 'gadtrec1b] }
  where
    a             = VarT (mkName "a")
    names@[v1,v2] = map mkName ["v1","v2"]
    [v1K,v2K]     = map (\n -> KindedTV n starK) names

#if MIN_VERSION_template_haskell(2,7,0)
gadtRecFamCI :: ConstructorInfo
gadtRecFamCI =
  ConstructorInfo
    { constructorName       = '(:#%:)
    , constructorVars       = []
    , constructorContext    = [ equalPred cTy (ConT ''Bool)
                              , equalPred dTy (ConT ''Bool)
                              ]
    , constructorFields     = [ConT ''Bool, ConT ''Bool]
    , constructorStrictness = [notStrictAnnot, notStrictAnnot]
    , constructorVariant    = RecordConstructor ['famRec1, 'famRec2] }
  where
    [cTy,dTy] = map (VarT . mkName) ["c", "d"]
#endif
