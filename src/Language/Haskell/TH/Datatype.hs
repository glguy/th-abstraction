{-# Language CPP, DeriveGeneric, DeriveDataTypeable #-}

{-|
Module      : Language.Haskell.TH.Datatype
Description : Backwards-compatible interface to reified information about datatypes.
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a flattened view of information about data types
and newtypes that can be supported uniformly across multiple versions
of the template-haskell package.

Sample output for @'reifyDatatype' ''Maybe@

@
'DatatypeInfo'
 { 'datatypeContext' = []
 , 'datatypeName'    = GHC.Base.Maybe
 , 'datatypeVars'    = [ 'VarT' a_3530822107858468866 ]
 , 'datatypeVariant' = 'Datatype'
 , 'datatypeCons'    =
     [ 'ConstructorInfo'
         { 'constructorName'    = GHC.Base.Nothing
         , 'constructorVars'    = []
         , 'constructorContext' = []
         , 'constructorFields'  = []
         , 'constructorVariant' = 'NormalConstructor'
         }
     , 'ConstructorInfo'
         { 'constructorName'    = GHC.Base.Just
         , 'constructorVars'    = []
         , 'constructorContext' = []
         , 'constructorFields'  = [ 'VarT' a_3530822107858468866 ]
         , 'constructorVariant' = 'NormalConstructor'
         }
     ]
 }
@

Datatypes declared with GADT syntax are normalized to constructors with existentially
quantified type variables and equality constraints.

-}
module Language.Haskell.TH.Datatype
  (
  -- * Types
    DatatypeInfo(..)
  , ConstructorInfo(..)
  , DatatypeVariant(..)
  , ConstructorVariant(..)

  -- * Normalization functions
  , reifyDatatype
  , normalizeInfo
  , normalizeDec
  , normalizeCon

  -- * Type variable manipulation
  , TypeSubstitution(..)
  , quantifyType
  , freshenFreeVariables

  -- * 'Pred' functions
  , equalPred
  , classPred

  -- * Backward compatible data definitions
  , dataDCompat
  , arrowKCompat

  -- * Type simplification
  , resolveTypeSynonyms
  , resolveInfixT

  -- * Convenience functions
  , unifyTypes
  , tvName
  , datatypeType
  , showFixity
  , showFixityDirection
  ) where

import           Data.Data (Typeable, Data)
import           Data.Foldable (foldMap, foldl')
import           Data.List (find, union, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Control.Monad (foldM)
import           GHC.Generics (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib (arrowK) -- needed for th-2.4

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative(..), (<$>))
import           Data.Traversable (traverse, sequenceA)
#endif

-- | Normalized information about newtypes and data types.
data DatatypeInfo = DatatypeInfo
  { datatypeContext :: Cxt               -- ^ Data type context (deprecated)
  , datatypeName    :: Name              -- ^ Type constructor
  , datatypeVars    :: [Type]            -- ^ Type parameters
  , datatypeVariant :: DatatypeVariant   -- ^ Extra information
  , datatypeCons    :: [ConstructorInfo] -- ^ Normalize constructor information
  }
  deriving (Show, Eq, Typeable, Data, Generic)

-- | Possible variants of data type declarations.
data DatatypeVariant
  = Datatype -- ^ Type declared with @data@
  | Newtype  -- ^ Type declared with @newtype@
  | DataInstance -- ^ Type declared with @data instance@
  | NewtypeInstance -- ^ Type declared with @newtype instance@
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | Normalized information about constructors associated with newtypes and
-- data types.
data ConstructorInfo = ConstructorInfo
  { constructorName    :: Name               -- ^ Constructor name
  , constructorVars    :: [TyVarBndr]        -- ^ Constructor type parameters
  , constructorContext :: Cxt                -- ^ Constructor constraints
  , constructorFields  :: [Type]             -- ^ Constructor fields
  , constructorVariant :: ConstructorVariant -- ^ Extra information
  }
  deriving (Show, Eq, Typeable, Data, Generic)

-- | Possible variants of data constructors.
data ConstructorVariant
  = NormalConstructor        -- ^ Constructor without field names
  | RecordConstructor [Name] -- ^ Constructor with field names
  deriving (Show, Eq, Ord, Typeable, Data, Generic)


-- | Construct a Type using the datatype's type constructor and type
-- parameters.
datatypeType :: DatatypeInfo -> Type
datatypeType di
  = foldl AppT (ConT (datatypeName di))
  $ datatypeVars di


-- | Compute a normalized view of the metadata about a data type or newtype
-- given a type constructor.
reifyDatatype ::
  Name {- ^ type constructor -} ->
  Q DatatypeInfo
reifyDatatype n = normalizeInfo =<< reify n


-- | Normalize 'Info' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
normalizeInfo :: Info -> Q DatatypeInfo
normalizeInfo (TyConI dec) = normalizeDec dec
# if MIN_VERSION_template_haskell(2,11,0)
normalizeInfo (DataConI name ty parent) = reifyParent name ty parent
# else
normalizeInfo (DataConI name ty parent _) = reifyParent name ty parent
# endif
normalizeInfo _ = fail "reifyDatatype: Expected a type constructor"


reifyParent :: Name -> Type -> Name -> Q DatatypeInfo
reifyParent con ty parent =
  do info <- reify parent
     case info of
       TyConI dec -> normalizeDec dec
       FamilyI dec instances ->
         do let instances1 = map (repairInstance dec ty) instances
            instances2 <- traverse normalizeDec instances1
            case find p instances2 of
              Just inst -> return inst
              Nothing   -> fail "PANIC: reifyParent lost the instance"
       _ -> fail "PANIC: reifyParent unexpected parent"
  where
    p info = con `elem` map constructorName (datatypeCons info)

#if MIN_VERSION_template_haskell(2,8,0) && (!MIN_VERSION_template_haskell(2,10,0))
    kindPart (KindedTV _ k) = [k]
    kindPart (PlainTV  _  ) = []

    countKindVars = length . freeVariables . map kindPart
    -- GHC 7.8.4 will eta-reduce data instances. We can find the missing
    -- type variables on the data constructor.
    repairInstance
      (FamilyD _ _ dvars _)
      (ForallT tvars _ _)
      (NewtypeInstD cx n ts con deriv) =
        NewtypeInstD cx n ts' con deriv
      where
        nparams = length dvars
        kparams = countKindVars dvars
        ts'     = take nparams (drop kparams (ts ++ bndrParams tvars))
    repairInstance
      (FamilyD _ _ dvars _)
      (ForallT tvars _ _)
      (DataInstD cx n ts cons deriv) =
        DataInstD cx n ts' cons deriv
      where
        nparams = length dvars
        kparams = countKindVars dvars
        ts'     = take nparams (drop kparams (ts ++ bndrParams tvars))
#endif
    repairInstance _ _ x = x


-- | Normalize 'Dec' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
normalizeDec :: Dec -> Q DatatypeInfo
#if MIN_VERSION_template_haskell(2,12,0)
normalizeDec (NewtypeD context name tyvars _kind con _derives) =
  normalizeDec' context name (bndrParams tyvars) [con] Newtype
normalizeDec (DataD context name tyvars _kind cons _derives) =
  normalizeDec' context name (bndrParams tyvars) cons Datatype
normalizeDec (NewtypeInstD context name params _kind con _derives) =
  repair13618 =<<
  normalizeDec' context name params [con] NewtypeInstance
normalizeDec (DataInstD context name params _kind cons _derives) =
  repair13618 =<<
  normalizeDec' context name params cons DataInstance
#elif MIN_VERSION_template_haskell(2,11,0)
normalizeDec (NewtypeD context name tyvars _kind con _derives) =
  normalizeDec' context name (bndrParams tyvars) [con] Newtype
normalizeDec (DataD context name tyvars _kind cons _derives) =
  normalizeDec' context name (bndrParams tyvars) cons Datatype
normalizeDec (NewtypeInstD context name params _kind con _derives) =
  repair13618 =<<
  normalizeDec' context name params [con] NewtypeInstance
normalizeDec (DataInstD context name params _kind cons _derives) =
  repair13618 =<<
  normalizeDec' context name params cons DataInstance
#else
normalizeDec (NewtypeD context name tyvars con _derives) =
  normalizeDec' context name (bndrParams tyvars) [con] Newtype
normalizeDec (DataD context name tyvars cons _derives) =
  normalizeDec' context name (bndrParams tyvars) cons Datatype
normalizeDec (NewtypeInstD context name params con _derives) =
  repair13618 =<<
  normalizeDec' context name params [con] NewtypeInstance
normalizeDec (DataInstD context name params cons _derives) =
  repair13618 =<<
  normalizeDec' context name params cons DataInstance
#endif
normalizeDec _ = fail "reifyDatatype: DataD or NewtypeD required"

bndrParams :: [TyVarBndr] -> [Type]
bndrParams = map $ \bndr ->
  case bndr of
    KindedTV t k -> SigT (VarT t) k
    PlainTV  t   -> VarT t


normalizeDec' ::
  Cxt             {- ^ Datatype context    -} ->
  Name            {- ^ Type constructor    -} ->
  [Type]          {- ^ Type parameters     -} ->
  [Con]           {- ^ Constructors        -} ->
  DatatypeVariant {- ^ Extra information   -} ->
  Q DatatypeInfo
normalizeDec' context name params cons variant =
  do cons' <- concat <$> traverse (normalizeCon name params) cons
     pure DatatypeInfo
       { datatypeContext = context
       , datatypeName    = name
       , datatypeVars    = params
       , datatypeCons    = cons'
       , datatypeVariant = variant
       }

-- | Normalize a 'Con' into a 'ConstructorInfo'. This requires knowledge of
-- the type and parameters of the constructor as extracted from the outer
-- 'Dec'.
normalizeCon ::
  Name   {- ^ Type constructor -} ->
  [Type] {- ^ Type parameters  -} ->
  Con    {- ^ Constructor      -} ->
  Q [ConstructorInfo]
normalizeCon typename params = go [] []
  where
    go tyvars context c =
      case c of
        NormalC n xs ->
          pure [ConstructorInfo n tyvars context (map snd xs) NormalConstructor]
        InfixC l n r ->
          pure [ConstructorInfo n tyvars context [snd l,snd r] NormalConstructor]
        RecC n xs ->
          let fns = takeFieldNames xs in
          pure [ConstructorInfo n tyvars context
                  (takeFieldTypes xs) (RecordConstructor fns)]
        ForallC tyvars' context' c' ->
          go (tyvars'++tyvars) (context'++context) c'

#if MIN_VERSION_template_haskell(2,11,0)
        GadtC ns xs innerType ->
          gadtCase ns innerType (map snd xs) NormalConstructor
        RecGadtC ns xs innerType ->
          let fns = takeFieldNames xs in
          gadtCase ns innerType (takeFieldTypes xs) (RecordConstructor fns)
      where
        gadtCase = normalizeGadtC typename params tyvars context


normalizeGadtC ::
  Name               {- ^ Type constructor             -} ->
  [Type]             {- ^ Type parameters              -} ->
  [TyVarBndr]        {- ^ Constructor parameters       -} ->
  Cxt                {- ^ Constructor context          -} ->
  [Name]             {- ^ Constructor names            -} ->
  Type               {- ^ Declared type of constructor -} ->
  [Type]             {- ^ Constructor field types      -} ->
  ConstructorVariant {- ^ Constructor variant          -} ->
  Q [ConstructorInfo]
normalizeGadtC typename params tyvars context names innerType fields variant =
  do innerType' <- resolveTypeSynonyms innerType
     case decomposeType innerType' of
       ConT innerTyCon :| ts | typename == innerTyCon ->

         let (substName, context1) = mergeArguments params ts
             subst   = VarT <$> substName
             tyvars' = [ tv | tv <- tyvars, Map.notMember (tvName tv) subst ]

             context2 = applySubstitution subst (context1 ++ context)
             fields'  = applySubstitution subst fields
         in pure [ConstructorInfo name tyvars' context2 fields' variant
                 | name <- names]

       _ -> fail "normalizeGadtC: Expected type constructor application"

mergeArguments ::
  [Type] {- ^ outer parameters                    -} ->
  [Type] {- ^ inner parameters (specializations ) -} ->
  (Map Name Name, Cxt)
mergeArguments ns ts = foldr aux (Map.empty, []) (zip ns ts)
  where
    aux (SigT x _, y) sc = aux (x,y) sc -- learn about kinds??
    aux (x, SigT y _) sc = aux (x,y) sc

    aux (f `AppT` x, g `AppT` y) sc =
      aux (x,y) (aux (f,g) sc)

    aux (VarT n,p) (subst, context) =
      case p of
        VarT m | Map.notMember m subst -> (Map.insert m n subst, context)
        _ -> (subst, EqualityT `AppT` VarT n `AppT` p : context)

    aux _ sc = sc
#endif

-- | Expand all of the type synonyms in a type.
resolveTypeSynonyms :: Type -> Q Type
resolveTypeSynonyms t =
  let f :| xs = decomposeType t

      notTypeSynCase = foldl AppT f <$> traverse resolveTypeSynonyms xs in

  case f of
    ConT n ->
      do info <- reify n
         case info of
           TyConI (TySynD _ synvars def) ->
             let argNames    = map tvName synvars
                 (args,rest) = splitAt (length argNames) xs
                 subst       = Map.fromList (zip argNames args)
                 t'          = foldl AppT (applySubstitution subst def) rest
             in resolveTypeSynonyms t'

           _ -> notTypeSynCase
    _ -> notTypeSynCase

-- | Decompose a type into a list of it's outermost applications. This process
-- forgets about infix application and explicit parentheses.
--
-- This operation should be used after all 'UInfixT' cases have been resolved
-- by 'resolveFixities' if the argument is being user generated.
--
-- > t ~= foldl1 AppT (decomposeType t)
decomposeType :: Type -> NonEmpty Type
decomposeType = go []
  where
    go args (AppT f x) = go (x:args) f
    go args t          = t :| args

-- 'NonEmpty' didn't move into base until recently. Reimplementing it locally
-- saves dependencies for supporting older GHCs
data NonEmpty a = a :| [a]


-- | Resolve any infix type application in a type using the fixities that
-- are currently available. Starting in `template-haskell-2.11` types could
-- contain unresolved infix applications.
resolveInfixT :: Type -> Q Type

#if MIN_VERSION_template_haskell(2,11,0)
resolveInfixT (ForallT vs cx t) = forallT vs (traverse resolveInfixT cx) (resolveInfixT t)
resolveInfixT (f `AppT` x)      = resolveInfixT f `appT` resolveInfixT x
resolveInfixT (ParensT t)       = resolveInfixT t
resolveInfixT (InfixT l o r)    = conT o `appT` resolveInfixT l `appT` resolveInfixT r
resolveInfixT (SigT t k)        = SigT <$> resolveInfixT t <*> resolveInfixT k
resolveInfixT t@UInfixT{}       = resolveInfixT =<< resolveInfixT1 (gatherUInfixT t)
resolveInfixT t                 = pure t

gatherUInfixT :: Type -> InfixList
gatherUInfixT (UInfixT l o r) = ilAppend (gatherUInfixT l) o (gatherUInfixT r)
gatherUInfixT t = ILNil t

-- This can fail due to incompatible fixities
resolveInfixT1 :: InfixList -> TypeQ
resolveInfixT1 = go []
  where
    go :: [(Type,Name,Fixity)] -> InfixList -> TypeQ
    go ts (ILNil u) = pure (foldl (\acc (l,o,_) -> ConT o `AppT` l `AppT` acc) u ts)
    go ts (ILCons l o r) =
      do ofx <- fromMaybe defaultFixity <$> reifyFixity o
         let push = go ((l,o,ofx):ts) r
         case ts of
           (l1,o1,o1fx):ts' ->
             case compareFixity o1fx ofx of
               Just True  -> go ((ConT o1 `AppT` l1 `AppT` l, o, ofx):ts') r
               Just False -> push
               Nothing    -> fail (precedenceError o1 o1fx o ofx)
           _ -> push

    compareFixity :: Fixity -> Fixity -> Maybe Bool
    compareFixity (Fixity n1 InfixL) (Fixity n2 InfixL) = Just (n1 >= n2)
    compareFixity (Fixity n1 InfixR) (Fixity n2 InfixR) = Just (n1 >  n2)
    compareFixity (Fixity n1 _     ) (Fixity n2 _     ) =
      case compare n1 n2 of
        GT -> Just True
        LT -> Just False
        EQ -> Nothing

    precedenceError :: Name -> Fixity -> Name -> Fixity -> String
    precedenceError o1 ofx1 o2 ofx2 =
      "Precedence parsing error: cannot mix ‘" ++
      nameBase o1 ++ "’ [" ++ showFixity ofx1 ++ "] and ‘" ++
      nameBase o2 ++ "’ [" ++ showFixity ofx2 ++
      "] in the same infix type expression"

data InfixList = ILCons Type Name InfixList | ILNil Type

ilAppend :: InfixList -> Name -> InfixList -> InfixList
ilAppend (ILNil l)         o r = ILCons l o r
ilAppend (ILCons l1 o1 r1) o r = ILCons l1 o1 (ilAppend r1 o r)

#else
-- older template-haskell packages don't have UInfixT
resolveInfixT = pure
#endif


-- | Render a 'Fixity' as it would appear in Haskell source.
--
-- Example: @infixl 5@
showFixity :: Fixity -> String
showFixity (Fixity n d) = showFixityDirection d ++ " " ++ show n


-- | Render a 'FixityDirection' like it would appear in Haskell source.
--
-- Examples: @infixl@ @infixr@ @infix@
showFixityDirection :: FixityDirection -> String
showFixityDirection InfixL = "infixl"
showFixityDirection InfixR = "infixr"
showFixityDirection InfixN = "infix"


-- | Extract the type variable name from a 'TyVarBndr' ignoring the
-- kind signature if one exists.
tvName :: TyVarBndr -> Name
tvName (PlainTV  name  ) = name
tvName (KindedTV name _) = name

takeFieldNames :: [(Name,a,b)] -> [Name]
takeFieldNames xs = [a | (a,_,_) <- xs]

takeFieldTypes :: [(a,b,Type)] -> [Type]
takeFieldTypes xs = [a | (_,_,a) <- xs]

------------------------------------------------------------------------

-- | Add universal quantifier for all free variables in the type. This is
-- useful when constructing a type signature for a declaration.
-- This code is careful to ensure that the order of the variables quantified
-- is determined by their order of appearance in the type signature. (In
-- contrast with being dependent upon the Ord instance for 'Name')
quantifyType :: Type -> Type
quantifyType t
  | null vs   = t
  | otherwise = ForallT (PlainTV <$> vs) [] t
  where
    vs = freeVariables t


-- | Substitute all of the free variables in a type with fresh ones
freshenFreeVariables :: Type -> Q Type
freshenFreeVariables t =
  do let xs = [ (n, VarT <$> newName (nameBase n)) | n <- freeVariables t]
     subst <- sequenceA (Map.fromList xs)
     return (applySubstitution subst t)


-- | Class for types that support type variable substitution.
class TypeSubstitution a where
  -- | Apply a type variable substitution
  applySubstitution :: Map Name Type -> a -> a
  -- | Compute the free type variables
  freeVariables     :: a -> [Name]

instance TypeSubstitution a => TypeSubstitution [a] where
  freeVariables     = foldMap freeVariables
  applySubstitution = fmap . applySubstitution

instance TypeSubstitution Type where
  applySubstitution subst = go
    where
      go (ForallT tvs context t) =
        let subst' = foldl' (flip Map.delete) subst (map tvName tvs) in
        ForallT tvs (applySubstitution subst' context)
                    (applySubstitution subst' t)
      go (AppT f x)      = AppT (go f) (go x)
      go (SigT t k)      = SigT (go t) (applySubstitution subst k) -- k could be Kind
      go (VarT v)        = Map.findWithDefault (VarT v) v subst
#if MIN_VERSION_template_haskell(2,11,0)
      go (InfixT l c r)  = InfixT (go l) c (go r)
      go (UInfixT l c r) = UInfixT (go l) c (go r)
      go (ParensT t)     = ParensT (go t)
#endif
      go t               = t

  freeVariables t =
    case t of
      ForallT tvs context t' ->
          (freeVariables context `union` freeVariables t')
          \\ map tvName tvs
      AppT f x      -> freeVariables f `union` freeVariables x
      SigT t' k     -> freeVariables t' `union` freeVariables k
      VarT v        -> [v]
#if MIN_VERSION_template_haskell(2,11,0)
      InfixT l _ r  -> freeVariables l `union` freeVariables r
      UInfixT l _ r -> freeVariables l `union` freeVariables r
      ParensT t'    -> freeVariables t'
#endif
      _             -> []

instance TypeSubstitution ConstructorInfo where
  freeVariables ci =
      (freeVariables (constructorContext ci) `union`
       freeVariables (constructorFields ci))
      \\ (tvName <$> constructorVars ci)

  applySubstitution subst ci =
    let subst' = foldl' (flip Map.delete) subst (map tvName (constructorVars ci)) in
    ci { constructorContext = applySubstitution subst' (constructorContext ci)
       , constructorFields  = applySubstitution subst' (constructorFields ci)
       }

-- 'Pred' became a type synonym for 'Type'
#if !MIN_VERSION_template_haskell(2,10,0)
instance TypeSubstitution Pred where
  freeVariables (ClassP _ xs) = freeVariables xs
  freeVariables (EqualP x y) = freeVariables x `union` freeVariables y

  applySubstitution p (ClassP n xs) = ClassP n (applySubstitution p xs)
  applySubstitution p (EqualP x y) = EqualP (applySubstitution p x)
                                            (applySubstitution p y)
#endif

-- 'Kind' became a type synonym for 'Type'. Previously there were no kind variables
#if !MIN_VERSION_template_haskell(2,8,0)
instance TypeSubstitution Kind where
  freeVariables _ = []
  applySubstitution _ k = k
#endif

------------------------------------------------------------------------

combineSubstitutions :: Map Name Type -> Map Name Type -> Map Name Type
combineSubstitutions x y = Map.union (fmap (applySubstitution y) x) y

-- | Compute the type variable substitution that unifies a list of types,
-- or fail in 'Q'.
unifyTypes :: [Type] -> Q (Map Name Type)
unifyTypes [] = pure Map.empty
unifyTypes (t:ts) =
  do t':ts' <- traverse resolveTypeSynonyms (t:ts)
     let aux sub u =
           do sub' <- unify' (applySubstitution sub t')
                             (applySubstitution sub u)
              return (combineSubstitutions sub sub')

     case foldM aux Map.empty ts' of
       Right m -> return m
       Left (x,y) ->
         fail $ showString "Unable to unify types "
              . showsPrec 11 x
              . showString " and "
              . showsPrec 11 y
              $ ""

unify' :: Type -> Type -> Either (Type,Type) (Map Name Type)

unify' (VarT n) (VarT m) | n == m = pure Map.empty
unify' (VarT n) t | n `elem` freeVariables t = Left (VarT n, t)
                  | otherwise                = pure (Map.singleton n t)
unify' t (VarT n) | n `elem` freeVariables t = Left (VarT n, t)
                  | otherwise                = pure (Map.singleton n t)

unify' (ConT n) (ConT m) | n == m = pure Map.empty

unify' (AppT f1 x1) (AppT f2 x2) =
  do sub1 <- unify' f1 f2
     sub2 <- unify' (applySubstitution sub1 x1) (applySubstitution sub1 x2)
     return (combineSubstitutions sub1 sub2)

unify' (TupleT n) (TupleT m) | n == m = pure Map.empty

unify' t u = Left (t,u)


-- | Construct an equality constraint. The implementation of 'Pred' varies
-- across versions of Template Haskell.
equalPred :: Type -> Type -> Pred
equalPred x y =
#if MIN_VERSION_template_haskell(2,10,0)
  AppT (AppT EqualityT x) y
#else
  EqualP x y
#endif

-- | Construct a typeclass constraint. The implementation of 'Pred' varies
-- across versions of Template Haskell.
classPred :: Name {- ^ class -} -> [Type] {- ^ parameters -} -> Pred
classPred =
#if MIN_VERSION_template_haskell(2,10,0)
  foldl AppT . ConT
#else
  ClassP
#endif

------------------------------------------------------------------------

-- | Prior to GHC 8.2.1, reify was broken for data instances and newtype
-- instances. This code attempts to detect the problem and repair it if
-- possible.
--
-- The particular problem is that the type variables used in the patterns
-- while defining a data family instance do not completely match those
-- used when defining the fields of the value constructors beyond the
-- base names. This code attempts to recover the relationship between the
-- type variables.
--
-- It is possible, however, to generate these kinds of declarations by
-- means other than reify. In these cases the name bases might not be
-- unique and the declarations might be well formed. In such a case this
-- code attempts to avoid altering the declaration.
--
-- https://ghc.haskell.org/trac/ghc/ticket/13618
repair13618 :: DatatypeInfo -> Q DatatypeInfo
repair13618 info =
  do s <- sequenceA (Map.fromList substList)
     return info { datatypeCons = applySubstitution s (datatypeCons info) }

  where
    used  = freeVariables (datatypeCons info)
    bound = freeVariables (datatypeVars info)
    free  = used \\ bound

    substList =
      [ (u, substEntry u vs)
      | u <- free
      , let vs = [v | v <- bound, nameBase v == nameBase u]
      ]

    substEntry _ [v] = varT v
    substEntry u []  = fail ("Impossible free variable: " ++ show u)
    substEntry u _   = fail ("Ambiguous free variable: "  ++ show u)

------------------------------------------------------------------------

-- | Backward compatible version of 'dataD'
dataDCompat ::
  CxtQ        {- ^ context                 -} ->
  Name        {- ^ type constructor        -} ->
  [TyVarBndr] {- ^ type parameters         -} ->
  [ConQ]      {- ^ constructor definitions -} ->
  [Name]      {- ^ derived class names     -} ->
  DecQ
#if MIN_VERSION_template_haskell(2,12,0)
dataDCompat c n ts cs ds =
  dataD c n ts Nothing cs
    (if null ds then [] else [derivClause Nothing (map conT ds)])
#elif MIN_VERSION_template_haskell(2,11,0)
dataDCompat c n ts cs ds =
  dataD c n ts Nothing cs
    (pure (map ConT ds))
#else
dataDCompat = dataD
#endif

arrowKCompat :: Kind -> Kind -> Kind
#if MIN_VERSION_template_haskell(2,8,0)
arrowKCompat x y = arrowK `appK` x `appK` y
#else
arrowKCompat = arrowK
#endif
