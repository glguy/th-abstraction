{-# Language CPP, FlexibleContexts, TypeFamilies, KindSignatures, TemplateHaskell, GADTs #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

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

#if __GLASGOW_HASKELL__ >= 704
import           Control.Monad (zipWithM_)
#endif

import           Control.Monad (unless)
import qualified Data.Map as Map

#if MIN_VERSION_base(4,7,0)
import           Data.Type.Equality ((:~:)(..))
#endif

import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
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
#if MIN_VERSION_template_haskell(2,6,0)
     t43Test
#endif
#if MIN_VERSION_template_haskell(2,7,0)
     dataFamilyTest
     ghc78bugTest
     quotedTest
     polyTest
     gadtFamTest
     famLocalDecTest1
     famLocalDecTest2
     recordFamTest
     t46Test
#endif
     fixityLookupTest
#if __GLASGOW_HASKELL__ >= 704
     resolvePredSynonymsTest
#endif
     reifyDatatypeWithConNameTest
     reifyConstructorTest
#if MIN_VERSION_base(4,7,0)
     importedEqualityTest
#endif
#if MIN_VERSION_template_haskell(2,8,0)
     kindSubstTest
     t59Test
     t61Test
#endif
#if __GLASGOW_HASKELL__ >= 800
     t37Test
     polyKindedExTyvarTest
#endif
     regressionTest44
     t63Test

adt1Test :: IO ()
adt1Test =
  $(do info <- reifyDatatype ''Adt1

       let vars@[a,b]  = map (VarT . mkName) ["a","b"]
           [aSig,bSig] = map (\v -> SigT v starK) vars

       validateDI info
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

       validateDI info
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

       let con = gadtRecVanillaCI

       validateDI info
         DatatypeInfo
           { datatypeName    = ''Gadtrec1
           , datatypeContext = []
           , datatypeVars    = [SigT (VarT (mkName "a")) starK]
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

       validateDI info
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

       validateDI info
         DatatypeInfo
           { datatypeName    = ''Showable
           , datatypeContext = []
           , datatypeVars    = []
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'Showable
                   , constructorVars       = [KindedTV a starK]
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
       validateDI info
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
       validateDI info
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
       validateDI info
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

recordVanillaTest :: IO ()
recordVanillaTest =
  $(do info <- reifyRecord 'gadtrec1a
       validateCI info gadtRecVanillaCI)

#if MIN_VERSION_template_haskell(2,6,0)
t43Test :: IO ()
t43Test =
  $(do [decPlain] <- [d| data T43Plain where MkT43Plain :: T43Plain |]
       infoPlain  <- normalizeDec decPlain
       validateDI infoPlain
         DatatypeInfo
           { datatypeName    = mkName "T43Plain"
           , datatypeContext = []
           , datatypeVars    = []
           , datatypeVariant = Datatype
           , datatypeCons    =
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
           { datatypeName    = mkName "T43Fam"
           , datatypeContext = []
           , datatypeVars    = []
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = mkName "MkT43Fam"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )
#endif

#if MIN_VERSION_template_haskell(2,7,0)
dataFamilyTest :: IO ()
dataFamilyTest =
  $(do info <- reifyDatatype 'DFMaybe
       let a = mkName "a"
       validateDI info
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
       validateDI info
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

quotedTest :: IO ()
quotedTest =
  $(do [dec] <- [d| data instance Quoted a = MkQuoted a |]
       info  <- normalizeDec dec
       let a = VarT (mkName "a")
       validateDI info
         DatatypeInfo
           { datatypeName    = mkName "Quoted"
           , datatypeContext = []
           , datatypeVars    = [SigT a starK]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = mkName "MkQuoted"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [a]
                   , constructorStrictness = [notStrictAnnot]
                   , constructorVariant    = NormalConstructor } ]
           }
  )

polyTest :: IO ()
polyTest =
  $(do info <- reifyDatatype 'MkPoly
       let [a,k] = map mkName ["a","k"]
       validateDI info
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
       validateDI info
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
                   , constructorVars       = [KindedTV e starK]
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
                   , constructorVars       = [KindedTV q starK]
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
           { datatypeName    = ''FamLocalDec1
           , datatypeContext = []
           , datatypeVars    = [ConT ''Int]
           , datatypeVariant = DataInstance
           , datatypeCons    =
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
       let tys@[a,b]   = map (VarT . mkName) ["a", "b"]
           [aSig,bSig] = map (\v -> SigT v starK) tys
       validateDI info
         DatatypeInfo
           { datatypeName    = ''FamLocalDec2
           , datatypeContext = []
           , datatypeVars    = [ConT ''Int, TupleT 2 `AppT` a `AppT` b, aSig]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = mkName "FamLocalDec2Int"
                   , constructorVars       = []
                   , constructorContext    = []
                   , constructorFields     = [TupleT 2 `AppT` b `AppT` a, ConT ''Int]
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

#endif

fixityLookupTest :: IO ()
fixityLookupTest =
  $(do Just (Fixity 6 InfixR) <- reifyFixityCompat '(:**:)
       [| return () |])

#if __GLASGOW_HASKELL__ >= 704
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
#endif

reifyDatatypeWithConNameTest :: IO ()
reifyDatatypeWithConNameTest =
  $(do info <- reifyDatatype 'Just
       validateDI info
         DatatypeInfo
          { datatypeContext = []
          , datatypeName    = ''Maybe
          , datatypeVars    = [SigT (VarT (mkName "a")) starK]
          , datatypeVariant = Datatype
          , datatypeCons    =
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

#if MIN_VERSION_base(4,7,0)
importedEqualityTest :: IO ()
importedEqualityTest =
  $(do info <- reifyDatatype ''(:~:)
       let [a,b] = map (VarT . mkName) ["a","b"]
           k     = mkName "k"
           kKind = varKCompat k
       validateDI info
         DatatypeInfo
           { datatypeContext = []
           , datatypeName    = ''(:~:)
           , datatypeVars    = [SigT a kKind, SigT b kKind]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'Refl
                   , constructorVars       = []
                   , constructorContext    = [equalPred a b]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )
#endif

#if MIN_VERSION_template_haskell(2,8,0)
kindSubstTest :: IO ()
kindSubstTest =
  $(do k1 <- newName "k1"
       k2 <- newName "k2"
       a  <- newName "a"
       let ty = ForallT [KindedTV a (VarT k1)] [] (VarT a)
           substTy = applySubstitution (Map.singleton k1 (VarT k2)) ty

           checkFreeVars :: Type -> [Name] -> Q ()
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
#if __GLASGOW_HASKELL__ >= 800
                        [PlainTV k, KindedTV a (VarT k)]
#else
                        [KindedTV a (VarT k)]
#endif
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
  $(do let test :: Type -> Type -> Q ()
           test orig expected = do
             actual <- resolveTypeSynonyms orig
             unless (expected == actual) $
               fail $ "Type synonym expansion failed: "
                   ++ unlines [ "Expected: " ++ pprint expected
                              , "Actual:   " ++ pprint actual
                              ]

           idAppT = (ConT ''Id `AppT`)
           a = mkName "a"
       test (SigT (idAppT $ ConT ''Int) (idAppT StarT))
            (SigT (ConT ''Int) StarT)
#if MIN_VERSION_template_haskell(2,10,0)
       test (ForallT [KindedTV a (idAppT StarT)]
                     [idAppT (ConT ''Show `AppT` VarT a)]
                     (idAppT $ VarT a))
            (ForallT [KindedTV a StarT]
                     [ConT ''Show `AppT` VarT a]
                     (VarT a))
#endif
#if MIN_VERSION_template_haskell(2,11,0)
       test (InfixT (idAppT $ ConT ''Int) ''Either (idAppT $ ConT ''Int))
            (InfixT (ConT ''Int) ''Either (ConT ''Int))
       test (ParensT (idAppT $ ConT ''Int))
            (ConT ''Int)
#endif
       [| return () |])
#endif

#if __GLASGOW_HASKELL__ >= 800
t37Test :: IO ()
t37Test =
  $(do infoA <- reifyDatatype ''T37a
       let [k,a] = map (VarT . mkName) ["k","a"]
       validateDI infoA
         DatatypeInfo
           { datatypeContext = []
           , datatypeName    = ''T37a
           , datatypeVars    = [SigT k starK, SigT a k]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'MkT37a
                   , constructorVars       = []
                   , constructorContext    = [equalPred k (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       infoB <- reifyDatatype ''T37b
       validateDI infoB
         DatatypeInfo
           { datatypeContext = []
           , datatypeName    = ''T37b
           , datatypeVars    = [SigT a k]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'MkT37b
                   , constructorVars       = []
                   , constructorContext    = [equalPred k (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }

       infoC <- reifyDatatype ''T37c
       validateDI infoC
         DatatypeInfo
           { datatypeContext = []
           , datatypeName    = ''T37c
           , datatypeVars    = [SigT a k]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'MkT37c
                   , constructorVars       = []
                   , constructorContext    = [equalPred a (ConT ''Bool)]
                   , constructorFields     = []
                   , constructorStrictness = []
                   , constructorVariant    = NormalConstructor } ]
           }
   )

polyKindedExTyvarTest :: IO ()
polyKindedExTyvarTest =
  $(do info <- reifyDatatype ''T48
       let [a,x] = map mkName ["a","x"]
       validateDI info
         DatatypeInfo
           { datatypeContext = []
           , datatypeName    = ''T48
           , datatypeVars    = [SigT (VarT a) starK]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName       = 'MkT48
                   , constructorVars       = [KindedTV x (VarT a)]
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
         DatatypeInfo { datatypeVars = [SigT (VarT a1) starK]
                      , datatypeCons =
                          [ ConstructorInfo
                              { constructorVars = [KindedTV _ (VarT a2)] } ] } ->
           unless (a1 == a2) $
             fail $ "Two occurrences of the same variable have different names: "
                 ++ show [a1, a2]
       [| return () |]
   )
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
           sigmaType = ForallT [PlainTV b] [] tauType
           expected = ForallT [PlainTV a, PlainTV b] [] tauType
           actual   = quantifyType sigmaType
       unless (expected == actual) $
         fail $ "quantifyType does not collapse consecutive foralls: "
             ++ unlines [ "Expected: " ++ pprint expected
                        , "Actual:   " ++ pprint actual
                        ]
       [| return () |])
