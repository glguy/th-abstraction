{-# Language CPP, FlexibleContexts, TypeFamilies, KindSignatures, TemplateHaskell, GADTs, RankNTypes, MagicHash, ConstraintKinds, PolyKinds #-}

#if __GLASGOW_HASKELL__ < 806
{-# Language TypeInType #-}
#endif

#if __GLASGOW_HASKELL__ >= 807
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
#endif

#if MIN_VERSION_template_haskell(2,21,0)
{-# Language TypeAbstractions #-}
#endif

#if MIN_VERSION_template_haskell(2,18,0)
{-# LANGUAGE UnliftedDatatypes #-}
#endif

-- We should aim to enable -Wincomplete-uni-patterns long-term. See #121.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

import           Control.Monad (unless, when, zipWithM_)
import qualified Data.Map as Map
import           Data.Kind
import           Data.Type.Equality ((:~:)(..))

#if __GLASGOW_HASKELL__ >= 810
import           GHC.Exts (Any, RuntimeRep(..), TYPE)
#endif
#if __GLASGOW_HASKELL__ >= 902
import           GHC.Exts (UnliftedType, Levity(..))
#endif

import           GHC.Exts (Array#)

import qualified Language.Haskell.TH as TH (Type)
import           Language.Haskell.TH hiding (Type)
import           Language.Haskell.TH.Datatype as Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib (starK)

import           Harness
import           Types

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
     recordVanillaTest
     t43Test
     t58Test
     dataFamilyTest
     ghc78bugTest
     quotedTest
     polyTest
     gadtFamTest
     famLocalDecTest1
     famLocalDecTest2
     recordFamTest
     t46Test
     t73Test
     t95Test
     fixityLookupTest
     resolvePredSynonymsTest
     reifyDatatypeWithConNameTest
     reifyConstructorTest
     importedEqualityTest
     kindSubstTest
     t59Test
     t61Test
     t66Test
     t80Test
     t79TestA
#if MIN_VERSION_template_haskell(2,19,0)
     t79TestB
#endif
     t37Test
     polyKindedExTyvarTest
#if __GLASGOW_HASKELL__ >= 807
     resolveTypeSynonymsVKATest
#endif
     regressionTest44
     t63Test
     t70Test
     t88Test
     captureAvoidanceTest
#if MIN_VERSION_template_haskell(2,20,0)
     t100Test
#endif
#if MIN_VERSION_template_haskell(2,21,0)
     t103Test
#endif
#if __GLASGOW_HASKELL__ >= 810
     t107Test
     t108Test
#endif
#if __GLASGOW_HASKELL__ >= 804
     t110Test
#endif
#if MIN_VERSION_template_haskell(2,16,0)
     unboxedTupleTest
#endif
#if MIN_VERSION_template_haskell(2,18,0)
     unliftedGADTDecTest
#endif
     primTyConTest


adt1Test :: IO ()
adt1Test =
  $(do info <- reifyDatatype ''Adt1

       let names            = map mkName ["a","b"]
           [aTvb,bTvb]      = map (\v -> kindedTV v starK) names
           vars@[aVar,bVar] = map (VarT . mkName) ["a","b"]
           [aSig,bSig]      = map (\v -> SigT v starK) vars

       validateDI info
         DatatypeInfo
           { datatypeName = ''Adt1
           , datatypeContext = []
           , datatypeVars = [aTvb,bTvb]
           , datatypeInstTypes = [aSig, bSig]
           , datatypeVariant = Datatype
           , datatypeReturnKind = starK
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Adtc1
                   , constructorContext = []
                   , constructorVars = []
                   , constructorFields = [AppT (AppT (TupleT 2) aVar) bVar]
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

       let a = mkName "a"
           aVar = VarT a

       validateDI info
         DatatypeInfo
           { datatypeName = ''Gadt1
           , datatypeContext = []
           , datatypeVars = [kindedTV a starK]
           , datatypeInstTypes = [SigT aVar starK]
           , datatypeVariant = Datatype
           , datatypeReturnKind = starK
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Gadtc1
                   , constructorVars = []
                   , constructorContext = [equalPred aVar (ConT ''Int)]
                   , constructorFields = [ConT ''Int]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = 'Gadtc2
                   , constructorVars = []
                   , constructorContext = []
                   , constructorFields = [AppT (AppT (TupleT 2) aVar) aVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = '(:**:)
                   , constructorVars = []
                   , constructorContext = [equalPred aVar (TupleT 0)]
                   , constructorFields = [ConT ''Bool, ConT ''Char]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = InfixConstructor }
               , ConstructorInfo
                   { constructorName = '(:!!:)
                   , constructorVars = []
                   , constructorContext = [equalPred aVar (ConT ''Double)]
                   , constructorFields = [ConT ''Char, ConT ''Bool]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant = NormalConstructor }
               ]
           }
   )

gadtrec1Test :: IO ()
gadtrec1Test =
  $(do info <- reifyDatatype ''Gadtrec1

       let a   = mkName "a"
           con = gadtRecVanillaCI

       validateDI info
         DatatypeInfo
           { datatypeName      = ''Gadtrec1
           , datatypeContext   = []
           , datatypeVars      = [kindedTV a starK]
           , datatypeInstTypes = [SigT (VarT a) starK]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ con, con { constructorName = 'Gadtrecc2 } ]
           }
   )

equalTest :: IO ()
equalTest =
  $(do info <- reifyDatatype ''Equal

       let names                 = map mkName ["a","b","c"]
           [aTvb,bTvb,cTvb]      = map (\v -> kindedTV v starK) names
           vars@[aVar,bVar,cVar] = map VarT names
           [aSig,bSig,cSig]      = map (\v -> SigT v starK) vars

       validateDI info
         DatatypeInfo
           { datatypeName      = ''Equal
           , datatypeContext   = []
           , datatypeVars      = [aTvb, bTvb, cTvb]
           , datatypeInstTypes = [aSig, bSig, cSig]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'Equalc
                   , constructorVars       = []
                   , constructorContext    =
                        [ equalPred aVar cVar
                        , equalPred bVar cVar
                        , classPred ''Read [cVar]
                        , classPred ''Show [cVar]
                        ]
                   , constructorFields     =
                        [ListT `AppT` cVar, ConT ''Maybe `AppT` cVar]
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

       validateDI info
         DatatypeInfo
           { datatypeName      = ''Showable
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'Showable
                   , constructorVars       = [kindedTV a starK]
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
       validateDI info
         DatatypeInfo
           { datatypeName      = ''R
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
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
       let names            = map mkName ["a","b"]
           [aTvb,bTvb]      = map (\v -> kindedTV v starK) names
           vars@[aVar,bVar] = map VarT names
           [aSig,bSig]      = map (\v -> SigT v starK) vars
           x                = mkName "x"

           con = ConstructorInfo
                     { constructorName       = undefined
                     , constructorVars       = []
                     , constructorContext    = []
                     , constructorFields     = []
                     , constructorStrictness = []
                     , constructorVariant    = NormalConstructor }
       validateDI info
         DatatypeInfo
           { datatypeName      = ''Gadt2
           , datatypeContext   = []
           , datatypeVars      = [aTvb, bTvb]
           , datatypeInstTypes = [aSig, bSig]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ con { constructorName = 'Gadt2c1
                     , constructorContext = [equalPred bVar (AppT ListT aVar)] }
               , con { constructorName = 'Gadt2c2
                     , constructorContext = [equalPred aVar (AppT ListT bVar)] }
               , con { constructorName = 'Gadt2c3
                     , constructorVars = [kindedTV x starK]
                     , constructorContext =
                         [equalPred aVar (AppT ListT (VarT x))
                         ,equalPred bVar (AppT ListT (VarT x))] } ]
           }
  )

voidstosTest :: IO ()
voidstosTest =
  $(do info <- reifyDatatype ''VoidStoS
       let g = mkName "g"
       validateDI info
         DatatypeInfo
           { datatypeName      = ''VoidStoS
           , datatypeContext   = []
           , datatypeVars      = [kindedTV g (arrowKCompat starK starK)]
           , datatypeInstTypes = [SigT (VarT g) (arrowKCompat starK starK)]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      = []
           }
  )

strictDemoTest :: IO ()
strictDemoTest =
  $(do info <- reifyDatatype ''StrictDemo
       validateDI info
         DatatypeInfo
           { datatypeName      = ''StrictDemo
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
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

recordVanillaTest :: IO ()
recordVanillaTest =
  $(do info <- reifyRecord 'gadtrec1a
       validateCI info gadtRecVanillaCI)

t43Test :: IO ()
t43Test =
  $(do [decPlain] <- [d| data T43Plain where MkT43Plain :: T43Plain |]
       infoPlain  <- normalizeDec decPlain
       validateDI infoPlain
         DatatypeInfo
           { datatypeName      = mkName "T43Plain"
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT43Plain"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       [decFam] <- [d| data instance T43Fam where  MkT43Fam :: T43Fam |]
       infoFam  <- normalizeDec decFam
       validateDI infoFam
         DatatypeInfo
           { datatypeName      = mkName "T43Fam"
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT43Fam"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )

t58Test :: IO ()
t58Test =
  $(do [dec] <- [d| data Foo where
                      MkFoo :: a -> Foo |]
       info <- normalizeDec dec
       let a = mkName "a"
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "Foo"
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = []
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkFoo"
                   , constructorVars       = [plainTV a]
                   , constructorContext    = []
                   , constructorFields     = [VarT a]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

dataFamilyTest :: IO ()
dataFamilyTest =
  $(do info <- reifyDatatype 'DFMaybe
       let a = mkName "a"
       validateDI info
         DatatypeInfo
           { datatypeName      = ''DF
           , datatypeContext   = []
           , datatypeVars      = [kindedTV a starK]
           , datatypeInstTypes = [AppT (ConT ''Maybe) (VarT a)]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
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
       let c    = mkName "c"
           cVar = VarT c
       validateDI info
         DatatypeInfo
           { datatypeName      = ''DF1
           , datatypeContext   = []
           , datatypeVars      = [kindedTV c starK]
           , datatypeInstTypes = [SigT cVar starK]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'DF1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [cVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

quotedTest :: IO ()
quotedTest =
  $(do [dec] <- [d| data instance Quoted a = MkQuoted a |]
       info  <- normalizeDec dec
       let a    = mkName "a"
           aVar = VarT a
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "Quoted"
           , datatypeContext   = []
           , datatypeVars      = [plainTV a]
           , datatypeInstTypes = [aVar]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkQuoted"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [aVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

polyTest :: IO ()
polyTest =
  $(do info <- reifyDatatype 'MkPoly
       let [a,k] = map mkName ["a","k"]
           kVar  = VarT k
       validateDI info
         DatatypeInfo
           { datatypeName      = ''Poly
           , datatypeContext   = []
           , datatypeVars      = [kindedTV k starK, kindedTV a kVar]
           , datatypeInstTypes = [SigT (VarT a) kVar]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
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
       let names@[c,d,e,q]       = map mkName ["c","d","e","q"]
           [cTvb,dTvb,eTvb,qTvb] = map (\v -> kindedTV v starK) names
           [cTy,dTy,eTy,qTy]     = map VarT names
           [cSig,dSig]           = map (\v -> SigT v starK) [cTy,dTy]
       validateDI info
         DatatypeInfo
           { datatypeName      = ''GadtFam
           , datatypeContext   = []
           , datatypeVars      = [cTvb,dTvb]
           , datatypeInstTypes = [cSig,dSig]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkGadtFam1
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [dTy,cTy]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor }
               , ConstructorInfo
                   { constructorName       = '(:&&:)
                   , constructorVars       = [kindedTV e starK]
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
               , gadtRecFamCI
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
                   , constructorVars       = [kindedTV q starK]
                   , constructorContext    = [ equalPred cTy (ConT ''Bool)
                                             , equalPred dTy (ConT ''Bool)
                                             , equalPred qTy (ConT ''Char)
                                             ]
                   , constructorFields     = [qTy]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

famLocalDecTest1 :: IO ()
famLocalDecTest1 =
  $(do [dec] <- [d| data instance FamLocalDec1 Int = FamLocalDec1Int { mochi :: Double } |]
       info <- normalizeDec dec
       validateDI info
         DatatypeInfo
           { datatypeName      = ''FamLocalDec1
           , datatypeContext   = []
           , datatypeVars      = []
           , datatypeInstTypes = [ConT ''Int]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "FamLocalDec1Int"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Double]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = RecordConstructor [mkName "mochi"] }]
           }
   )

famLocalDecTest2 :: IO ()
famLocalDecTest2 =
  $(do [dec] <- [d| data instance FamLocalDec2 Int (a, b) a = FamLocalDec2Int { fm0 :: (b, a), fm1 :: Int } |]
       info <- normalizeDec dec
       let names            = map mkName ["a", "b"]
           [aTvb,bTvb]      = map plainTV names
           vars@[aVar,bVar] = map (VarT . mkName) ["a", "b"]
       validateDI info
         DatatypeInfo
           { datatypeName      = ''FamLocalDec2
           , datatypeContext   = []
           , datatypeVars      = [aTvb,bTvb]
           , datatypeInstTypes = [ConT ''Int, TupleT 2 `AppT` aVar `AppT` bVar, aVar]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "FamLocalDec2Int"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [TupleT 2 `AppT` bVar `AppT` aVar, ConT ''Int]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = RecordConstructor [mkName "fm0", mkName "fm1"] }]
           }
   )

recordFamTest :: IO ()
recordFamTest =
  $(do info <- reifyRecord 'famRec1
       validateCI info gadtRecFamCI)

t46Test :: IO ()
t46Test =
  $(do info <- reifyDatatype 'MkT46
       case info of
         DatatypeInfo { datatypeCons = [ConstructorInfo { constructorContext = ctxt }]} ->
           unless (null ctxt) (fail "regression test for ticket #46 failed")
         _ -> fail "T46 should have exactly one constructor"
       [| return () |])

t73Test :: IO ()
t73Test =
  $(do info <- reifyDatatype 'MkT73
       let b    = mkName "b"
           bTvb = kindedTV b starK
           bVar = VarT b
       validateDI info
         DatatypeInfo
           { datatypeName      = ''T73
           , datatypeContext   = []
           , datatypeVars      = [bTvb]
           , datatypeInstTypes = [ConT ''Int, SigT bVar starK]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT73
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [bVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor }]
           }
   )

t95Test :: IO ()
t95Test =
  $(do info <- reifyDatatype 'MkT95
       let a    = mkName "a"
           aTvb = kindedTV a starK
           aVar = VarT a
       validateDI info
         DatatypeInfo
           { datatypeName      = ''T95
           , datatypeContext   = []
           , datatypeVars      = [aTvb]
           , datatypeInstTypes = [AppT ListT aVar]
           , datatypeVariant   = DataInstance
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT95
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [aVar]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor }]
           }
   )

fixityLookupTest :: IO ()
fixityLookupTest =
  $(do Just (Fixity 6 InfixR) <- reifyFixityCompat '(:**:)
       [| return () |])

resolvePredSynonymsTest :: IO ()
resolvePredSynonymsTest =
  $(do info <- reifyDatatype ''PredSynT
       [cxt1,cxt2,cxt3] <- sequence $ map (mapM resolvePredSynonyms . constructorContext)
                                    $ datatypeCons info
       let mkTest = zipWithM_ (equateCxt "resolvePredSynonymsTest")
           test1 = mkTest cxt1 [classPred ''Show [ConT ''Int]]
           test2 = mkTest cxt2 [classPred ''Show [ConT ''Int]]
           test3 = mkTest cxt3 [equalPred (ConT ''Int) (ConT ''Int)]
       mapM_ (either fail return) [test1,test2,test3]
       [| return () |])

reifyDatatypeWithConNameTest :: IO ()
reifyDatatypeWithConNameTest =
  $(do info <- reifyDatatype 'Just
       let a = mkName "a"
       validateDI info
         DatatypeInfo
          { datatypeContext   = []
          , datatypeName      = ''Maybe
          , datatypeVars      = [kindedTV a starK]
          , datatypeInstTypes = [SigT (VarT a) starK]
          , datatypeVariant   = Datatype
          , datatypeReturnKind = starK
          , datatypeCons      =
              [ ConstructorInfo
                  { constructorName       = 'Nothing
                  , constructorVars       = []
                  , constructorContext    = []
                  , constructorFields     = []
                  , constructorStrictness = []
                  , constructorVariant    = NormalConstructor
                  }
              , justCI
              ]
          }
   )

reifyConstructorTest :: IO ()
reifyConstructorTest =
  $(do info <- reifyConstructor 'Just
       validateCI info justCI)

importedEqualityTest :: IO ()
importedEqualityTest =
  $(do info <- reifyDatatype ''(:~:)
       let names@[a,b] = map mkName ["a","b"]
           [aVar,bVar] = map VarT names
           k           = mkName "k"
           kKind       = VarT k
       validateDI info
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''(:~:)
           , datatypeVars      = [ kindedTV k starK
                                 , kindedTV a kKind
                                 , kindedTV b kKind
                                 ]
           , datatypeInstTypes = [SigT aVar kKind, SigT bVar kKind]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'Refl
                   , constructorVars       = []
                   , constructorContext    = [equalPred aVar bVar]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )

kindSubstTest :: IO ()
kindSubstTest =
  $(do k1 <- newName "k1"
       k2 <- newName "k2"
       a  <- newName "a"
       let ty = ForallT [kindedTVSpecified a (VarT k1)] [] (VarT a)
           substTy = applySubstitution (Map.singleton k1 (VarT k2)) ty

           checkFreeVars :: TH.Type -> [Name] -> Q ()
           checkFreeVars t freeVars =
             unless (freeVariables t == freeVars) $
               fail $ "free variables of " ++ show t ++ " should be " ++ show freeVars

       checkFreeVars ty      [k1]
       checkFreeVars substTy [k2]
       [| return () |])

t59Test :: IO ()
t59Test =
  $(do k <- newName "k"
       a <- newName "a"
       let proxyAK  = ConT (mkName "Proxy") `AppT` SigT (VarT a) (VarT k)
                        -- Proxy (a :: k)
           expected = ForallT
                        [plainTVSpecified k, kindedTVSpecified a (VarT k)]
                        [] proxyAK
           actual = quantifyType proxyAK
       unless (expected == actual) $
         fail $ "quantifyType does not respect dependency order: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])

t61Test :: IO ()
t61Test =
  $(do let test :: TH.Type -> TH.Type -> Q ()
           test orig expected = do
             actual <- resolveTypeSynonyms orig
             unless (expected == actual) $
               fail $ "Type synonym expansion failed: "
                   ++ unlines [ "Expected: " ++ pprint expected
                              , "Actual:   " ++ pprint actual
                              ]

           idAppT = (ConT ''Id `AppT`)
           a = mkName "a"
       test (SigT (idAppT $ ConT ''Int) (idAppT starK))
            (SigT (ConT ''Int) starK)
       test (ForallT [kindedTVSpecified a (idAppT starK)]
                     [idAppT (ConT ''Show `AppT` VarT a)]
                     (idAppT $ VarT a))
            (ForallT [kindedTVSpecified a starK]
                     [ConT ''Show `AppT` VarT a]
                     (VarT a))
       test (InfixT (idAppT $ ConT ''Int) ''Either (idAppT $ ConT ''Int))
            (InfixT (ConT ''Int) ''Either (ConT ''Int))
       test (ParensT (idAppT $ ConT ''Int))
            (ConT ''Int)
#if MIN_VERSION_template_haskell(2,19,0)
       test (PromotedInfixT (idAppT $ ConT ''Int) '(:^:) (idAppT $ ConT ''Int))
            (PromotedInfixT (ConT ''Int) '(:^:) (ConT ''Int))
#endif
       [| return () |])

t66Test :: IO ()
t66Test =
  $(do [dec] <- [d| data Foo a b :: (* -> *) -> * -> * where
                      MkFoo :: a -> b -> f x -> Foo a b f x |]
       info <- normalizeDec dec
       let [a,b,f,x] = map mkName ["a","b","f","x"]
           fKind     = arrowKCompat starK starK
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "Foo"
           , datatypeContext   = []
           , datatypeVars      = [ plainTV a, plainTV b
                                 , kindedTV f fKind, kindedTV x starK ]
           , datatypeInstTypes = [ VarT a, VarT b
                                 , SigT (VarT f) fKind, SigT (VarT x) starK ]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkFoo"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [VarT a, VarT b, VarT f `AppT` VarT x]
                   , constructorStrictness = [notStrictAnnot, notStrictAnnot, notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
   )

t80Test :: IO ()
t80Test = do
  let [k,a,b] = map mkName ["k","a","b"]
      -- forall k (a :: k) (b :: k). ()
      t = ForallT [ plainTVSpecified k
                  , kindedTVSpecified a (VarT k)
                  , kindedTVSpecified b (VarT k)
                  ] [] (ConT ''())

      expected, actual :: [Name]
      expected = []
      actual   = freeVariables t

  unless (expected == actual) $
    fail $ "Bug in ForallT substitution: "
        ++ unlines [ "Expected: " ++ pprint expected
                   , "Actual:   " ++ pprint actual
                   ]
  return ()

t79TestA :: IO ()
t79TestA =
  $(do let [a,b,c]  = map mkName ["a","b","c"]
           t        = ForallT [kindedTVSpecified a (UInfixT (VarT b) ''(:+:) (VarT c))] []
                              (ConT ''())
           expected = ForallT [kindedTVSpecified a (ConT ''(:+:) `AppT` VarT b `AppT` VarT c)] []
                              (ConT ''())
       actual <- resolveInfixT t
       unless (expected == actual) $
         fail $ "resolveInfixT does not recur into the kinds of "
             ++ "ForallT type variable binders: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])

#if MIN_VERSION_template_haskell(2,19,0)
t79TestB :: IO ()
t79TestB =
  $(do let [a,b,c]  = map mkName ["a","b","c"]
           t        = ForallT [kindedTVSpecified a (PromotedUInfixT (VarT b) '(:^:) (VarT c))] []
                              (ConT ''())
           expected = ForallT [kindedTVSpecified a (PromotedT '(:^:) `AppT` VarT b `AppT` VarT c)] []
                              (ConT ''())
       actual <- resolveInfixT t
       unless (expected == actual) $
         fail $ "resolveInfixT does not recur into the kinds of "
             ++ "ForallT type variable binders: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])
#endif

t37Test :: IO ()
t37Test =
  $(do infoA <- reifyDatatype ''T37a
       let names@[k,a] = map mkName ["k","a"]
           [kVar,aVar] = map VarT names
           kSig        = SigT kVar starK
           aSig        = SigT aVar kVar
           kTvb        = kindedTV k starK
           aTvb        = kindedTV a kVar
       validateDI infoA
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T37a
           , datatypeVars      = [kTvb, aTvb]
           , datatypeInstTypes = [kSig, aSig]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT37a
                   , constructorVars       = []
                   , constructorContext    = [equalPred kVar (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       infoB <- reifyDatatype ''T37b
       validateDI infoB
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T37b
           , datatypeVars      = [kTvb, aTvb]
           , datatypeInstTypes = [aSig]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT37b
                   , constructorVars       = []
                   , constructorContext    = [equalPred kVar (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       infoC <- reifyDatatype ''T37c
       validateDI infoC
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T37c
           , datatypeVars      = [kTvb, aTvb]
           , datatypeInstTypes = [aSig]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT37c
                   , constructorVars       = []
                   , constructorContext    = [equalPred aVar (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )

polyKindedExTyvarTest :: IO ()
polyKindedExTyvarTest =
  $(do info <- reifyDatatype ''T48
       let [a,x] = map mkName ["a","x"]
           aVar  = VarT a
       validateDI info
         DatatypeInfo
           { datatypeContext   = []
           , datatypeName      = ''T48
           , datatypeVars      = [kindedTV a starK]
           , datatypeInstTypes = [SigT aVar starK]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = 'MkT48
                   , constructorVars       = [kindedTV x aVar]
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Prox `AppT` VarT x]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
       -- Because validateCI uses a type variable substitution to normalize
       -- away any alpha-renaming differences between constructors, it
       -- unfortunately does not check if the uses of `a` in datatypeVars and
       -- constructorVars are the same. We perform this check explicitly here.
       case info of
         DatatypeInfo { datatypeVars = [v1]
                      , datatypeCons =
                          [ConstructorInfo { constructorVars = [v2] }] }
           |  a1 <- tvName v1, starK == tvKind v1, VarT a2 <- tvKind v2
           -> unless (a1 == a2) $
                fail $ "Two occurrences of the same variable have different names: "
                    ++ show [a1, a2]
         _ -> fail $ "Unexpected DatatypeInfo for T48: "
                    ++ show info
       [| return () |]
   )

t75Test :: IO ()
t75Test =
  $(do info <- reifyDatatype ''T75
       case datatypeCons info of
         [c] -> let datatypeVarTypes    = map (VarT . tvName) $ datatypeVars info
                    constructorVarKinds = map tvKind $ constructorVars c in
                unless (datatypeVarTypes == constructorVarKinds) $
                  fail $ "Mismatch between datatypeVars and constructorVars' kinds: "
                      ++ unlines [ "datatypeVars:           "
                                     ++ pprint datatypeVarTypes
                                 , "constructorVars' kinds: "
                                     ++ pprint constructorVarKinds
                                 ]
         cs  -> fail $ "Unexpected number of constructors for T75: "
                    ++ show (length cs)
       [| return () |]
   )

#if __GLASGOW_HASKELL__ >= 807
resolveTypeSynonymsVKATest :: IO ()
resolveTypeSynonymsVKATest =
  $(do t  <- [t| T37b @Bool True |]
       t' <- resolveTypeSynonyms t
       unless (t == t') $
         fail $ "Type synonym expansion breaks with visible kind application: "
            ++ show [t, t']
       [| return () |])
#endif

regressionTest44 :: IO ()
regressionTest44 =
  $(do intToInt <- [t| Int -> Int |]
       unified  <- unifyTypes [intToInt, intToInt]
       unless (Map.null unified) (fail "regression test for ticket #44 failed")
       [| return () |])

t63Test :: IO ()
t63Test =
  $(do a <- newName "a"
       b <- newName "b"
       t <- newName "T"
       let tauType = ArrowT `AppT` VarT a `AppT` (ArrowT `AppT` VarT b
                       `AppT` (ConT t `AppT` VarT a))
           sigmaType = ForallT [plainTVSpecified b] [] tauType
           expected = ForallT [plainTVSpecified a, plainTVSpecified b] [] tauType
           actual   = quantifyType sigmaType
       unless (expected == actual) $
         fail $ "quantifyType does not collapse consecutive foralls: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])

t70Test :: IO ()
t70Test =
  $(do a <- newName "a"
       b <- newName "b"
       let [aVar, bVar] = map VarT    [a, b]
           [aTvb, bTvb] = map plainTV [a, b]
       let fvsABExpected = [aTvb, bTvb]
           fvsABActual   = freeVariablesWellScoped [aVar, bVar]

           fvsBAExpected = [bTvb, aTvb]
           fvsBAActual   = freeVariablesWellScoped [bVar, aVar]

           check expected actual =
             unless (expected == actual) $
               fail $ "freeVariablesWellScoped does not preserve left-to-right order: "
                   ++ unlines [ "Expected: " ++ pprint expected
                              , "Actual:   " ++ pprint actual
                              ]

       check fvsABExpected fvsABActual
       check fvsBAExpected fvsBAActual

       [| return () |])

t88Test :: IO ()
t88Test =
  $(do let unexpandedType = ConT ''Id
           expected       = unexpandedType
       actual <- resolveTypeSynonyms (ConT ''Id)
       unless (expected == actual) $
         fail $ "resolveTypeSynonyms incorrectly expands an undersaturated type synonym: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])

captureAvoidanceTest :: IO ()
captureAvoidanceTest = do
  let a        = mkName "a"
      b        = mkName "b"
      subst    = Map.singleton b (VarT a)
      origTy   = ForallT [plainTVSpecified a] [] (VarT b)
      substTy  = applySubstitution subst origTy
      wrongTy  = ForallT [plainTVSpecified a] [] (VarT a)
  when (substTy == wrongTy) $
    fail $ "applySubstitution captures during substitution"

#if MIN_VERSION_template_haskell(2,20,0)
t100Test :: IO ()
t100Test =
  $(do let expectedInfo =
             DatatypeInfo
               { datatypeName = ''T100
               , datatypeContext = []
               , datatypeVars = []
               , datatypeInstTypes = []
               , datatypeVariant = Datatype.TypeData
               , datatypeReturnKind = starK
               , datatypeCons =
                   [ ConstructorInfo
                       { constructorName = ''MkT100
                       , constructorContext = []
                       , constructorVars = []
                       , constructorFields = []
                       , constructorStrictness = []
                       , constructorVariant = NormalConstructor }
                   ]
               }

       t100Info <- reifyDatatype ''T100
       validateDI t100Info expectedInfo

       mkT100Info <- reifyDatatype ''MkT100
       validateDI mkT100Info expectedInfo
   )
#endif

#if MIN_VERSION_template_haskell(2,21,0)
t103Test :: IO ()
t103Test =
  $(do [dec] <- [d| data T102 @k (a :: k) |]
       info <- normalizeDec dec
       let k = mkName "k"
           a = mkName "a"
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "T102"
           , datatypeContext   = []
           , datatypeVars      = [plainTV k, kindedTV a (VarT k)]
           , datatypeInstTypes = [SigT (VarT a) (VarT k)]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      = []
           }
   )
#endif

#if __GLASGOW_HASKELL__ >= 810
t107Test :: IO ()
t107Test =
  $(do info <- reifyDatatype ''T107
       let r = mkName "r"
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "T107"
           , datatypeContext   = []
           , datatypeVars      = [kindedTV r (ConT ''RuntimeRep)]
           , datatypeInstTypes = []
           , datatypeVariant   = Newtype
           , datatypeReturnKind = ConT ''TYPE `AppT` VarT r
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT107"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [ConT ''Any `SigT` (ConT ''TYPE `AppT` VarT r)]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor
                   }
               ]
           }
   )

t108Test :: IO ()
t108Test =
  $(do [dec] <- [d| data T108 :: forall k -> k -> Type where
                      MkT108 :: forall k (a :: k). T108 k a
                  |]
       info <- normalizeDec dec
       let k = mkName "k"
           a = mkName "a"
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "T108"
           , datatypeContext   = []
           , datatypeVars      = [plainTV k, kindedTV a (VarT k)]
           , datatypeInstTypes = [VarT k, SigT (VarT a) (VarT k)]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT108"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor
                   }
               ]
           }
   )
#endif

#if __GLASGOW_HASKELL__ >= 804
t110Test :: IO ()
t110Test =
  $(do [dec] <- [d| data T110 :: forall k. k -> Type where
                      MkT110 :: forall k (a :: k). T110 a
                  |]
       info <- normalizeDec dec
       let k = mkName "k"
           a = mkName "a"
       validateDI info
         DatatypeInfo
           { datatypeName      = mkName "T110"
           , datatypeContext   = []
           , datatypeVars      = [plainTV k, kindedTV a (VarT k)]
           , datatypeInstTypes = [SigT (VarT a) (VarT k)]
           , datatypeVariant   = Datatype
           , datatypeReturnKind = starK
           , datatypeCons      =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT110"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor
                   }
               ]
           }
   )
#endif

#if MIN_VERSION_template_haskell(2,16,0)
unboxedTupleTest :: IO ()
unboxedTupleTest =
  $(do k0 <- newName "k0"
       k1 <- newName "k1"
       a <- newName "a"
       b  <- newName "b"
       tupleInfo <- reifyDatatype (unboxedTupleTypeName 2)
       validateDI tupleInfo
         DatatypeInfo
           { datatypeContext = []
           , datatypeName = unboxedTupleTypeName 2
           , datatypeVars = [kindedTV k0 starK
                            ,kindedTV a (AppT (ConT ''TYPE) (VarT k0 ))
                            ,kindedTV k1 starK
                            ,kindedTV b (AppT (ConT ''TYPE) (VarT k1))]
           , datatypeInstTypes = [SigT (VarT a) (AppT (ConT ''TYPE) (VarT k0))
                                 ,SigT (VarT b) (AppT (ConT ''TYPE) (VarT k1))]
           , datatypeVariant = Datatype
           , datatypeReturnKind =
               AppT
                 (ConT ''TYPE)
                 (AppT
                    (PromotedT 'TupleRep)
                    (AppT
                      (AppT PromotedConsT (VarT k0))
                        (AppT
                          (AppT PromotedConsT (VarT k1))
                          (SigT PromotedNilT (AppT ListT (ConT ''RuntimeRep))))))
           , datatypeCons =
             [ ConstructorInfo
               { constructorName = unboxedTupleDataName 2
               , constructorVars = []
               , constructorContext = []
               , constructorFields = [VarT a, VarT b]
               , constructorStrictness = [notStrictAnnot, notStrictAnnot]
               , constructorVariant = NormalConstructor}]
          }
  )
#endif

#if MIN_VERSION_template_haskell(2,18,0)
unliftedGADTDecTest :: IO ()
unliftedGADTDecTest =
  $(do a <- newName "a"
       s <- newName "s"
       [dec] <- [d| data UnliftedGADT a :: UnliftedType where
                      UnliftedGADT :: Show s => s -> a -> UnliftedGADT a
                |]
       info <- normalizeDec dec
       validateDI info
         DatatypeInfo
           { datatypeContext = []
           , datatypeName = mkName "UnliftedGADT"
           , datatypeVars = [plainTV a]
           , datatypeInstTypes = [VarT a]
           , datatypeVariant = Datatype
           , datatypeReturnKind = ConT ''TYPE `AppT` (PromotedT 'BoxedRep `AppT` PromotedT 'Unlifted)
           , datatypeCons =
               [ConstructorInfo
                  {constructorName = mkName "UnliftedGADT"
                  , constructorVars = [plainTV s]
                  , constructorContext = [AppT (ConT ''Show) (VarT s)]
                  , constructorFields = [VarT s,VarT a]
                  , constructorStrictness = [notStrictAnnot, notStrictAnnot]
                  , constructorVariant = NormalConstructor}
               ]
           }
   )
#endif


primTyConTest :: IO ()
primTyConTest =
  $(do l <- newName "l"
       a <- newName "a"
       info <- reifyDatatype ''Array#
       validateDI info
         DatatypeInfo
           { datatypeContext = []
           , datatypeName = mkName "Array#"
#if MIN_VERSION_template_haskell(2,19,0)
           , datatypeVars = [kindedTV l (ConT ''Levity)
                            , kindedTV a (ConT ''TYPE `AppT` (PromotedT 'BoxedRep `AppT` VarT l))
                            ]
           , datatypeInstTypes = [SigT (VarT a) (ConT ''TYPE `AppT` (PromotedT 'BoxedRep `AppT` VarT l))]
           , datatypeReturnKind = ConT ''TYPE `AppT` (PromotedT 'BoxedRep `AppT` PromotedT 'Unlifted)
#elif MIN_VERSION_template_haskell(2,18,0)
           , datatypeVars = [ kindedTV a StarT]
           , datatypeInstTypes = [SigT (VarT a) StarT]
           , datatypeReturnKind = ConT ''TYPE `AppT` (PromotedT 'BoxedRep `AppT` PromotedT 'Unlifted)
#elif MIN_VERSION_template_haskell(2,16,0)
           , datatypeVars = [kindedTV a starK]
           , datatypeInstTypes = [SigT (VarT a) starK]
           , datatypeReturnKind = ConT ''TYPE `AppT` PromotedT 'UnliftedRep
#else
           , datatypeVars = [kindedTV a starK]
           , datatypeInstTypes = [SigT (VarT a) starK]
           , datatypeReturnKind = starK
#endif
           , datatypeVariant = Datatype
           , datatypeCons = []
           }
   )
