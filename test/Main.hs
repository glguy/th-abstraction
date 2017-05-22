{-# Language CPP, PolyKinds, TypeFamilies, KindSignatures, TemplateHaskell, GADTs #-}

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

import Harness

type Gadt1Int = Gadt1 Int

data Gadt1 (a :: *) where
  Gadtc1 :: Int   -> Gadt1Int
  Gadtc2 :: (a,a) -> Gadt1 a

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

data family DF (a :: *)
data instance DF (Maybe a) = DFMaybe Int [a]

#if MIN_VERSION_template_haskell(2,9,0)
data family DF1 (a :: k)
#elif MIN_VERSION_template_haskell(2,8,0)
data family DF1 a
#else
data family DF1 (a :: *)
#endif
data instance DF1 b = DF1 b

data VoidStoS (f :: * -> *)

#if MIN_VERSION_template_haskell(2,9,0)
data family Poly (a :: k)
#elif MIN_VERSION_template_haskell(2,8,0)
data family Poly a
#else
data family Poly (a :: *)
#endif
data instance Poly a = MkPoly

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
     dataFamilyTest
     ghc78bugTest
     voidstosTest

adt1Test :: IO ()
adt1Test =
  $(do info <- reifyDatatype ''Adt1

       let [a,b] = map (VarT . mkName) ["a","b"]

       validate info
         DatatypeInfo
           { datatypeName = ''Adt1
           , datatypeContext = []
           , datatypeVars = [a, b]
           , datatypeVariant = Datatype
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Adtc1
                   , constructorContext = []
                   , constructorVars = []
                   , constructorFields = [AppT (AppT (TupleT 2) a) b]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = 'Adtc2
                   , constructorContext = []
                   , constructorVars = []
                   , constructorFields = [ConT ''Bool, ConT ''Int]
                   , constructorVariant = NormalConstructor }
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
           , datatypeVars = [a]
           , datatypeVariant = Datatype
           , datatypeCons =
               [ ConstructorInfo
                   { constructorName = 'Gadtc1
                   , constructorVars = []
                   , constructorContext = [equalPred a (ConT ''Int)]
                   , constructorFields = [ConT ''Int]
                   , constructorVariant = NormalConstructor }
               , ConstructorInfo
                   { constructorName = 'Gadtc2
                   , constructorVars = []
                   , constructorContext = []
                   , constructorFields = [AppT (AppT (TupleT 2) a) a]
                   , constructorVariant = NormalConstructor }
               ]
           }
   )

gadtrec1Test :: IO ()
gadtrec1Test =
  $(do info <- reifyDatatype ''Gadtrec1

       let a = VarT (mkName "a")
       let [v1,v2] = map mkName ["v1","v2"]

       let con = ConstructorInfo
                   { constructorName    = 'Gadtrecc1
                   , constructorVars    = [PlainTV v1, PlainTV v2]
                   , constructorContext =
                        [equalPred a (AppT (AppT (TupleT 2) (VarT v1)) (VarT v2))]
                   , constructorFields  = [VarT v1, VarT v2]
                   , constructorVariant = RecordConstructor ['gadtrec1a, 'gadtrec1b] }

       validate info
         DatatypeInfo
           { datatypeName    = ''Gadtrec1
           , datatypeContext = []
           , datatypeVars    = [a]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ con, con { constructorName = 'Gadtrecc2 } ]
           }
   )

equalTest :: IO ()
equalTest =
  $(do info <- reifyDatatype ''Equal

       let [a,b,c] = map (VarT . mkName) ["a","b","c"]

       validate info
         DatatypeInfo
           { datatypeName    = ''Equal
           , datatypeContext = []
           , datatypeVars    = [a, b, c]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName    = 'Equalc
                   , constructorVars    = []
                   , constructorContext =
                        [equalPred a c, equalPred b c, classPred ''Read [c], classPred ''Show [c] ]
                   , constructorFields  =
                        [ListT `AppT` c, ConT ''Maybe `AppT` c]
                   , constructorVariant = NormalConstructor }
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
                   { constructorName    = 'Showable
                   , constructorVars    = [PlainTV a]
                   , constructorContext = [classPred ''Show [VarT a]]
                   , constructorFields  = [VarT a]
                   , constructorVariant = NormalConstructor }
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
                   { constructorName    = 'R1
                   , constructorVars    = []
                   , constructorContext = []
                   , constructorFields  = [ConT ''Int, ConT ''Int]
                   , constructorVariant = RecordConstructor ['field1, 'field2] }
               ]
           }
   )

gadt2Test :: IO ()
gadt2Test =
  $(do info <- reifyDatatype ''Gadt2
       let [a,b] = map (VarT . mkName) ["a","b"]
           x     = mkName "x"
           con   = ConstructorInfo
                     { constructorName    = undefined
                     , constructorVars    = []
                     , constructorContext = []
                     , constructorFields  = []
                     , constructorVariant = NormalConstructor }
       validate info
         DatatypeInfo
           { datatypeName    = ''Gadt2
           , datatypeContext = []
           , datatypeVars    = [a, b]
           , datatypeVariant = Datatype
           , datatypeCons    =
               [ con { constructorName = 'Gadt2c1
                     , constructorContext = [equalPred b (AppT ListT a)] }
               , con { constructorName = 'Gadt2c2
                     , constructorContext = [equalPred a (AppT ListT b)] }
               , con { constructorName = 'Gadt2c3
                     , constructorVars = [PlainTV x]
                     , constructorContext =
                         [equalPred a (AppT ListT (VarT x))
                         ,equalPred b (AppT ListT (VarT x))] } ]
           }
  )

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
                   { constructorName    = 'DFMaybe
                   , constructorVars    = []
                   , constructorContext = []
                   , constructorFields  = [ConT ''Int, ListT `AppT` VarT a]
                   , constructorVariant = NormalConstructor } ]
           }
  )

ghc78bugTest :: IO ()
ghc78bugTest =
  $(do info <- reifyDatatype 'DF1
       let c = mkName "c"
       validate info
         DatatypeInfo
           { datatypeName    = ''DF1
           , datatypeContext = []
           , datatypeVars    = [VarT c]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName    = 'DF1
                   , constructorVars    = []
                   , constructorContext = []
                   , constructorFields  = [VarT c]
                   , constructorVariant = NormalConstructor } ]
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
           , datatypeVars    = [VarT g]
           , datatypeVariant = Datatype
           , datatypeCons    = []
           }
  )

polyTest :: IO ()
polyTest =
  $(do info <- reifyDatatype 'MkPoly
       let a = mkName "a"
       validate info
         DatatypeInfo
           { datatypeName    = ''Poly
           , datatypeContext = []
           , datatypeVars    = [VarT a]
           , datatypeVariant = DataInstance
           , datatypeCons    =
               [ ConstructorInfo
                   { constructorName    = 'MkPoly
                   , constructorVars    = []
                   , constructorContext = []
                   , constructorFields  = []
                   , constructorVariant = NormalConstructor } ]
           }
  )
