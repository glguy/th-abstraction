{-# Language TemplateHaskell #-}
module Harness where

import Control.Monad
import qualified Data.Map as Map
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

validate :: DatatypeInfo -> DatatypeInfo -> ExpQ
validate x y = either fail (\_ -> [| return () |]) (equateDI x y)

equateDI :: DatatypeInfo -> DatatypeInfo -> Either String ()
equateDI dat1 dat2 =
  do check "datatypeName"     datatypeName            dat1 dat2
     check "datatypeVars len" (length . datatypeVars) dat1 dat2
     check "datatypeVariant"  datatypeVariant         dat1 dat2
     check "datatypeCons len" (length . datatypeCons) dat1 dat2

     let sub = Map.fromList (zip (freeVariables (datatypeVars dat2))
                                 (map VarT (freeVariables (datatypeVars dat1))))

     check "datatypeContext" id
       (datatypeContext dat1)
       (applySubstitution sub (datatypeContext dat2))

     check "datatypeVars" id
       (datatypeVars dat1)
       (applySubstitution sub (datatypeVars dat2))

     zipWithM_ equateCI
       (datatypeCons dat1)
       (applySubstitution sub (datatypeCons dat2))

equateCI :: ConstructorInfo -> ConstructorInfo -> Either String ()
equateCI con1 con2 =
  do check "constructorName"     constructorName    con1 con2
     check "constructorVariant"  constructorVariant con1 con2

     let sub = Map.fromList (zip (map tvName (constructorVars con2))
                                 (map VarT (map tvName (constructorVars con1))))

     check "constructorContext" id
        (constructorContext con1)
        (applySubstitution sub (constructorContext con2))

     check "constructorFields" id
        (constructorFields con1)
        (applySubstitution sub (constructorFields con2))

check :: (Show b, Eq b) => String -> (a -> b) -> a -> a -> Either String ()
check lbl f x y
  | f x == f y = Right ()
  | otherwise  = Left (lbl ++ ":\n\n" ++ show (f x) ++ "\n\n" ++ show (f y))
