{-# Language CPP, TypeFamilies, KindSignatures, TemplateHaskell, GADTs #-}

#if MIN_VERSION_template_haskell(2,8,0)
{-# Language PolyKinds #-}
#endif

{-|
Module      : Main
Description : Test cases for the th-abstraction package
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module checks that the 'reifyDatatype' logic works consistently
across a wide range of datatypes. These tests are validated across
the versions of GHC supported by this package.

-}
module Main (main) where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib (starK)

import Harness

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


# if MIN_VERSION_template_haskell(2,8,0)
data family Poly (a :: k)
# else
data family Poly (a :: *)
# endif
data instance Poly a = MkPoly

data family GadtFam (a :: *) (b :: *)
data instance GadtFam c d where
  MkGadtFam1 :: x   -> y        -> GadtFam y x
  (:&&:)     :: e   -> f        -> GadtFam [e] f   -- This is declard infix
  (:^^:)     :: Int -> Int      -> GadtFam Int Int -- This is not
  MkGadtFam4 :: (Int ~ z) => z  -> GadtFam z z
  MkGadtFam5 :: (q ~ Char) => q -> GadtFam Bool Bool
infixl 3 :&&:

data family GadtLocalDec a
#endif

return [] -- segment type declarations above from refiy below

-- | Test entry point. Tests will pass or fail at compile time.
main :: IO ()
main =
  do adt1Test
     gadt1Test
     gadt2Test
     gadtrec1Test
     equalTest
     showableTest
     recordTest
     voidstosTest
     strictDemoTest
#if MIN_VERSION_template_haskell(2,7,0)
     dataFamilyTest
     ghc78bugTest
     polyTest
     gadtFamTest
     gadtDecTest
#endif
     fixityLookupTest

adt1Test :: IO ()
adt1Test =
  $(do info <- reifyDatatype ''Adt1

       let vars@[a,b]  = map (VarT . mkName) ["a","b"]
           [aSig,bSig] = map (\v -> SigT v starK) vars

       validate info
         DatatypeInfo
           { datatypeName = ''Adt1
           , datatypeContext = []
           , datatypeVars = [aSig, bSig]
           , datatypeVariant = Datatype
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Adtc1
                   , constructorContext = []
                   , constructorVars = []
                   , constructorFields = [AppT (AppT (TupleT 2) a) b]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = 'Adtc2
                   , constructorContext = []
                   , constructorVars = []
                   , constructorFields = [ConT ''Bool, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = InfixConstructor }
               ]
           }
   )

gadt1Test :: IO ()
gadt1Test =
  $(do info <- reifyDatatype ''Gadt1

       let a = VarT (mkName "a")

       validate info
         DatatypeInfo
           { datatypeName = ''Gadt1
           , datatypeContext = []
           , datatypeVars = [SigT a starK]
           , datatypeVariant = Datatype
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Gadtc1
                   , constructorVars = []
                   , constructorContext = [equalPred a (ConT ''Int)]
                   , constructorFields = [ConT ''Int]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = 'Gadtc2
                   , constructorVars = []
                   , constructorContext = []
                   , constructorFields = [AppT (AppT (TupleT 2) a) a]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = '(:**:)
                   , constructorVars = []
                   , constructorContext = [equalPred a (TupleT 0)]
                   , constructorFields = [ConT ''Bool, ConT ''Char]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = InfixConstructor }
               , ConstructorInfo
                   { constructorName = '(:!!:)
                   , constructorVars = []
                   , constructorContext = [equalPred a (ConT ''Double)]
                   , constructorFields = [ConT ''Char, ConT ''Bool]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               ]
           }
   )

gadtrec1Test :: IO ()
gadtrec1Test =
  $(do info <- reifyDatatype ''Gadtrec1

       let a             = VarT (mkName "a")
           names@[v1,v2] = map mkName ["v1","v2"]
           [v1K,v2K]     = map (\n -> KindedTV n starK) names

       let con = ConstructorInfo
                   { constructorName       = 'Gadtrecc1
                   , constructorVars       = [v1K, v2K]
                   , constructorContext    =
                        [equalPred a (AppT (AppT (TupleT 2) (VarT v1)) (VarT v2))]
                   , constructorFields     = [VarT v1, VarT v2]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = RecordConstructor ['gadtrec1a, 'gadtrec1b] }

       validate info
         DatatypeInfo
           { datatypeName    = ''Gadtrec1
           , datatypeContext = []
           , datatypeVars    = [SigT a starK]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ con, con { constructorName = 'Gadtrecc2 } ]
           }
   )

equalTest :: IO ()
equalTest =
  $(do info <- reifyDatatype ''Equal

       let vars@[a,b,c]     = map (VarT . mkName) ["a","b","c"]
           [aSig,bSig,cSig] = map (\v -> SigT v starK) vars

       validate info
         DatatypeInfo
           { datatypeName    = ''Equal
           , datatypeContext = []
           , datatypeVars    = [aSig, bSig, cSig]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'Equalc
                   , constructorVars       = []
                   , constructorContext    =
                        [equalPred a c, equalPred b c, classPred ''Read [c], classPred ''Show [c] ]
                   , constructorFields     =
                        [ListT `AppT` c, ConT ''Maybe `AppT` c]
                   , constructorStrictness =
                        [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               ]
           }
   )

showableTest :: IO ()
showableTest =
  $(do info <- reifyDatatype ''Showable

       let a = mkName "a"

       validate info
         DatatypeInfo
           { datatypeName    = ''Showable
           , datatypeContext = []
           , datatypeVars    = []
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'Showable
                   , constructorVars       = [PlainTV a]
                   , constructorContext    = [classPred ''Show [VarT a]]
                   , constructorFields     = [VarT a]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               ]
           }
   )

recordTest :: IO ()
recordTest =
  $(do info <- reifyDatatype ''R
       validate info
         DatatypeInfo
           { datatypeName    = ''R
           , datatypeContext = []
           , datatypeVars    = []
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'R1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Int, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = RecordConstructor ['field1, 'field2] }
               ]
           }
   )

gadt2Test :: IO ()
gadt2Test =
  $(do info <- reifyDatatype ''Gadt2
       let vars@[a,b]  = map (VarT . mkName) ["a","b"]
           [aSig,bSig] = map (\v -> SigT v starK) vars
           x     = mkName "x"
           con   = ConstructorInfo
                     { constructorName       = undefined
                     , constructorVars       = []
                     , constructorContext    = []
                     , constructorFields     = []
                     , constructorStrictness = []
                     , constructorVariant    = NormalConstructor }
       validate info
         DatatypeInfo
           { datatypeName    = ''Gadt2
           , datatypeContext = []
           , datatypeVars    = [aSig, bSig]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ con { constructorName = 'Gadt2c1
                     , constructorContext = [equalPred b (AppT ListT a)] }
               , con { constructorName = 'Gadt2c2
                     , constructorContext = [equalPred a (AppT ListT b)] }
               , con { constructorName = 'Gadt2c3
                     , constructorVars = [KindedTV x starK]
                     , constructorContext =
                         [equalPred a (AppT ListT (VarT x))
                         ,equalPred b (AppT ListT (VarT x))] } ]
           }
  )

voidstosTest :: IO ()
voidstosTest =
  $(do info <- reifyDatatype ''VoidStoS
       let g = mkName "g"
       validate info
         DatatypeInfo
           { datatypeName    = ''VoidStoS
           , datatypeContext = []
           , datatypeVars    = [SigT (VarT g) (arrowKCompat starK starK)]
           , datatypeVariant = Datatype
           , datatypeCons    = []
           }
  )

strictDemoTest :: IO ()
strictDemoTest =
  $(do info <- reifyDatatype ''StrictDemo
       validate info
         DatatypeInfo
           { datatypeName    = ''StrictDemo
           , datatypeContext = []
           , datatypeVars    = []
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'StrictDemo
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Int, ConT ''Int, ConT ''Int]
                   , constructorStrictness = [ notStrictAnnot
                                             , isStrictAnnot
                                             , unpackedAnnot
                                             ]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

#if MIN_VERSION_template_haskell(2,7,0)
dataFamilyTest :: IO ()
dataFamilyTest =
  $(do info <- reifyDatatype 'DFMaybe
       let a = mkName "a"
       validate info
         DatatypeInfo
           { datatypeName    = ''DF
           , datatypeContext = []
           , datatypeVars    = [AppT (ConT ''Maybe) (VarT a)]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'DFMaybe
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Int, ListT `AppT` VarT a]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

ghc78bugTest :: IO ()
ghc78bugTest =
  $(do info <- reifyDatatype 'DF1
       let c = VarT (mkName "c")
       validate info
         DatatypeInfo
           { datatypeName    = ''DF1
           , datatypeContext = []
           , datatypeVars    = [SigT c starK]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'DF1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [c]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

polyTest :: IO ()
polyTest =
  $(do info <- reifyDatatype 'MkPoly
       let [a,k] = map mkName ["a","k"]
       validate info
         DatatypeInfo
           { datatypeName    = ''Poly
           , datatypeContext = []
           , datatypeVars    = [SigT (VarT a) (varKCompat k)]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'MkPoly
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
  )

gadtFamTest :: IO ()
gadtFamTest =
  $(do info <- reifyDatatype 'MkGadtFam1
       let names@[c,d,e,q]   = map mkName ["c","d","e","q"]
           [cTy,dTy,eTy,qTy] = map VarT names
           [cSig,dSig]       = map (\v -> SigT v starK) [cTy,dTy]
       validate info
         DatatypeInfo
           { datatypeName    = ''GadtFam
           , datatypeContext = []
           , datatypeVars    = [cSig,dSig]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'MkGadtFam1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [dTy,cTy]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               , ConstructorInfo
                   { constructorName       = '(:&&:)
                   , constructorVars       = [PlainTV e]
                   , constructorContext    = [equalPred cTy (AppT ListT eTy)]
                   , constructorFields     = [eTy,dTy]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = InfixConstructor }
               , ConstructorInfo
                   { constructorName       = '(:^^:)
                   , constructorVars       = []
                   , constructorContext    = [ equalPred cTy (ConT ''Int)
                                             , equalPred dTy (ConT ''Int)
                                             ]
                   , constructorFields     = [ConT ''Int, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               , ConstructorInfo
                   { constructorName       = 'MkGadtFam4
                   , constructorVars       = []
                   , constructorContext    = [ equalPred cTy dTy
                                             , equalPred (ConT ''Int) dTy
                                             ]
                   , constructorFields     = [dTy]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               , ConstructorInfo
                   { constructorName       = 'MkGadtFam5
                   , constructorVars       = [PlainTV q]
                   , constructorContext    = [ equalPred cTy (ConT ''Bool)
                                             , equalPred dTy (ConT ''Bool)
                                             , equalPred qTy (ConT ''Char)
                                             ]
                   , constructorFields     = [qTy]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

gadtDecTest :: IO ()
gadtDecTest =
  $(do [dec] <- [d| data instance GadtLocalDec Int = GadtLocalDecInt { mochi :: Double } |]
       info <- normalizeDec dec
       validate info
         DatatypeInfo
           { datatypeName    = ''GadtLocalDec
           , datatypeContext = []
           , datatypeVars    = [ConT ''Int]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = mkName "GadtLocalDecInt"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Double]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = RecordConstructor [mkName "mochi"] }]
           }
   )
#endif

fixityLookupTest :: IO ()
fixityLookupTest =
  $(do Just (Fixity 6 InfixR) <- reifyFixityCompat '(:**:)
       [| return () |])
