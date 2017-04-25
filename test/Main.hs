{-# Language CPP, TemplateHaskell, GADTs #-}

module Main (main) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import System.Exit

type Gadt1Int = Gadt1 Int

data Gadt1 a where
  Gadtc1 :: Int   -> Gadt1Int
  Gadtc2 :: (a,a) -> Gadt1 a

data Adt1 a b = Adtc1 (a,b) | Adtc2 Bool Int

data Gadtrec1 a where
  Gadtrecc1 :: { gadtrec1a :: a, gadtrec1b :: b } -> Gadtrec1 (a,b)

return [] -- segment type declarations above from refiy below

main :: IO ()
main =
  do adt1Test
     gadt1Test
     gadtrec1Test

adt1Test :: IO ()
adt1Test =
  $(do info <- reifyDatatype ''Adt1

       let [a,b]   = map tvName (datatypeVars info)
           [c1,c2] = datatypeCons info

       unless (datatypeName info == ''Adt1) (fail "bad name adt1")
       unless (datatypeVariant info == Datatype) (fail "bad variant adt1")

       unless (c1 == ConstructorInfo 'Adtc1 [] [] [AppT (AppT (TupleT 2) (VarT a)) (VarT b)] NormalConstructor)
              (fail "Bad adtc1")

       unless (c2 == ConstructorInfo 'Adtc2 [] [] [ConT ''Bool, ConT ''Int] NormalConstructor)
              (fail "Bad adtc2")

       [| putStrLn "Adt1 tests passed" |]
   )

gadt1Test :: IO ()
gadt1Test =
  $(do info <- reifyDatatype ''Gadt1

       let [a]     = map tvName (datatypeVars info)
           [c1,c2] = datatypeCons info

       unless (null (constructorVars c1)) (fail "Bad vars c1")
       unless (constructorName c1 == 'Gadtc1) (fail "Bad name c1")
       unless (constructorVariant c1 == NormalConstructor) (fail "Bad variant c1")
       let [fi1] = constructorFields c1
       unless (fi1 == ConT ''Int) (fail "Bad field c1")

       unless (null (constructorVars c2)) (fail "Bad vars c2")
       unless (constructorName c2 == 'Gadtc2) (fail "Bad name c2")
       unless (constructorVariant c2 == NormalConstructor) (fail "Bad variant c2")
       let [fi2] = constructorFields c2
       unless (fi2 == AppT (AppT (TupleT 2) (VarT a)) (VarT a)) (fail "Bad field c2")

       [| putStrLn "Gadt1 tests passed" |]
   )

gadtrec1Test :: IO ()
gadtrec1Test =
  $(do info <- reifyDatatype ''Gadtrec1

       let [a] = map tvName (datatypeVars info)
           [c] = datatypeCons info
           [v1,v2] = constructorVars c

#if MIN_VERSION_template_haskell(2,10,0)
           expectedCxt = [AppT (AppT EqualityT (VarT a))
                               (AppT (AppT (TupleT 2) (VarT (tvName v1)))
                                     (VarT (tvName v2)))]
#else
           expectedCxt = [EqualP (VarT a)
                                 (AppT (AppT (TupleT 2) (VarT (tvName v1)))
                                       (VarT (tvName v2)))]

#endif

       unless (c == ConstructorInfo 'Gadtrecc1 [v1,v2] expectedCxt
                        [VarT (tvName v1), VarT (tvName v2)]
                        (RecordConstructor [ 'gadtrec1a, 'gadtrec1b ]))
              (fail ("bad constructor for gadtrec1" ++ show c))

       [| putStrLn "Gadtrecc1 tests" |]
   )
