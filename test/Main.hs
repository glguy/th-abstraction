{-# Language TemplateHaskell, GADTs #-}

module Main (main) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

type Gadt1Int = Gadt1 Int

data Gadt1 a where
  Gadtc1 :: Int   -> Gadt1Int
  Gadtc2 :: (a,a) -> Gadt1 a

data Adt1 a b = Adtc1 (a,b) | Bool `Adtc2` Int

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

       let [a,b]   = freeVariables (datatypeVars info)
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

       let [a]     = freeVariables (datatypeVars info)
           [c1,c2] = datatypeCons info

       unless (c1 == ConstructorInfo 'Gadtc1 [] [equalPred (VarT a) (ConT ''Int)]
                         [ConT ''Int] NormalConstructor)
              (fail ("bad Gadtc1 " ++ show c1))

       unless (c2 == ConstructorInfo 'Gadtc2 [] []
                        [AppT (AppT (TupleT 2) (VarT a)) (VarT a)]
                        NormalConstructor)
              (fail ("bad Gadtc2 " ++ show c2))

       [| putStrLn "Gadt1 tests passed" |]
   )

gadtrec1Test :: IO ()
gadtrec1Test =
  $(do info <- reifyDatatype ''Gadtrec1

       let [a] = freeVariables (datatypeVars info)
           [c] = datatypeCons info
           [v1,v2] = constructorVars c

           expectedCxt = [equalPred
                            (VarT a)
                            (AppT (AppT (TupleT 2) (VarT (tvName v1)))
                                  (VarT (tvName v2)))]

       unless (c == ConstructorInfo 'Gadtrecc1 [v1,v2] expectedCxt
                        [VarT (tvName v1), VarT (tvName v2)]
                        (RecordConstructor [ 'gadtrec1a, 'gadtrec1b ]))
              (fail ("bad constructor for gadtrec1" ++ show c))

       [| putStrLn "Gadtrecc1 tests" |]
   )
