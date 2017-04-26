{-# Language CPP, DeriveGeneric, DeriveDataTypeable #-}

{-|
Module      : Language.Haskell.TH.Datatype
Description : Backwards-compatible interface to reified information about datatypes.
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a flattened view of information about data types
and newtypes that can be supported uniformly across multiple verisons
of the template-haskell package.

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


  -- * Convenience functions
  , resolveTypeSynonyms
  , unifyTypes
  , tvName
  , datatypeType
  ) where

import           Data.Data (Typeable, Data)
import           Data.Foldable (foldMap, foldl')
import           Data.List (union, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad (foldM)
import           GHC.Generics (Generic)
import           Language.Haskell.TH

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative(..), (<$>))
import           Data.Traversable (traverse, sequenceA)
#endif

-- | Normalized information about newtypes and data types.
data DatatypeInfo = DatatypeInfo
  { datatypeContext :: Cxt               -- ^ Data type context (deprecated)
  , datatypeName    :: Name              -- ^ Type constructor
  , datatypeVars    :: [Type]            -- ^ Type parameters
  , datatypeDerives :: [Type]            -- ^ Derived constraints
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
  | InfixConstructor         -- ^ Infix constructor
  | RecordConstructor [Name] -- ^ Constructor with field names
  deriving (Show, Eq, Ord, Typeable, Data, Generic)


-- | Construct a Type using the datatype's type constructor and type
-- parameteters.
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
normalizeInfo _ = fail "reifyDatatype: Expected a type constructor"


-- | Normalize 'Dec' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
normalizeDec :: Dec -> Q DatatypeInfo
#if MIN_VERSION_template_haskell(2,12,0)
normalizeDec (NewtypeD context name tyvars _kind con derives) =
  normalizeDec' context name (bndrParams tyvars) [con] (extractDerivCxt derives) Newtype
normalizeDec (DataD context name tyvars _kind cons derives) =
  normalizeDec' context name (bndrParams tyvars) cons (extractDerivCxt derives) Datatype
normalizeDec (NewtypeInstD context name params _kind con derives) =
  normalizeDec' context name params [con] (extractDerivCxt derives) NewtypeInstance
normalizeDec (DataInstD context name params _kind cons derives) =
  normalizeDec' context name params cons (extractDerivCxt derives) DataInstance
#elif MIN_VERSION_template_haskell(2,11,0)
normalizeDec (NewtypeD context name tyvars _kind con derives) =
  normalizeDec' context name (bndrParams tyvars) [con] derives Newtype
normalizeDec (DataD context name tyvars _kind cons derives) =
  normalizeDec' context name (bndrParams tyvars) cons derives Datatype
normalizeDec (NewtypeInstD context name params _kind con derives) =
  normalizeDec' context name params [con] derives NewtypeInstance
normalizeDec (DataInstD context name params _kind cons derives) =
  normalizeDec' context name params cons derives DataInstance
#else
normalizeDec (NewtypeD context name tyvars con derives) =
  normalizeDec' context name (bndrParams tyvars) [con] (map ConT derives) Newtype
normalizeDec (DataD context name tyvars cons derives) =
  normalizeDec' context name (bndrParams tyvars) cons (map ConT derives) Datatype
normalizeDec (NewtypeInstD context name params con derives) =
  normalizeDec' context name params [con] (map ConT derives) NewtypeInstance
normalizeDec (DataInstD context name params cons derives) =
  normalizeDec' context name params cons (map ConT derives) DataInstance
#endif
normalizeDec _ = fail "reifyDatatype: DataD or NewtypeD required"

bndrParams :: [TyVarBndr] -> [Type]
bndrParams = map (VarT . tvName)

#if MIN_VERSION_template_haskell(2,12,0)
extractDerivCxt :: [DerivClause] -> Cxt
extractDerivCxt xs = [ c | DerivClause _ cs <- xs, c <- cs ]
#endif

normalizeDec' ::
  Cxt             {- ^ Datatype context    -} ->
  Name            {- ^ Type constructor    -} ->
  [Type]          {- ^ Type parameters     -} ->
  [Con]           {- ^ Constructors        -} ->
  [Type]          {- ^ Derived constraints -} ->
  DatatypeVariant {- ^ Extra information   -} ->
  Q DatatypeInfo
normalizeDec' context name params cons derives variant =
  do let vs = freeVariables params
     cons' <- concat <$> traverse (normalizeCon name vs) cons
     pure DatatypeInfo
       { datatypeContext = context
       , datatypeName    = name
       , datatypeVars    = params
       , datatypeCons    = cons'
       , datatypeDerives = derives
       , datatypeVariant = variant
       }

-- | Normalize a 'Con' into a 'ConstructorInfo'. This requires knowledge of
-- the type and parameters of the constructor as extracted from the outer
-- 'Dec'.
normalizeCon ::
  Name   {- ^ Type constructor -} ->
  [Name] {- ^ Type parameters  -} ->
  Con    {- ^ Constructor      -} ->
  Q [ConstructorInfo]
normalizeCon typename vars = go [] []
  where
    go tyvars context c =
      case c of
        NormalC n xs ->
          pure [ConstructorInfo n tyvars context (map snd xs) NormalConstructor]
        InfixC l n r ->
          pure [ConstructorInfo n tyvars context [snd l,snd r] InfixConstructor]
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
        gadtCase = normalizeGadtC typename vars tyvars context


normalizeGadtC ::
  Name               {- ^ Type constructor             -} ->
  [Name]             {- ^ Type parameters              -} ->
  [TyVarBndr]        {- ^ Constructor parameters       -} ->
  Cxt                {- ^ Constructor context          -} ->
  [Name]             {- ^ Constructor names            -} ->
  Type               {- ^ Declared type of constructor -} ->
  [Type]             {- ^ Constructor field types      -} ->
  ConstructorVariant {- ^ Constructor variant          -} ->
  Q [ConstructorInfo]
normalizeGadtC typename vars tyvars context names innerType fields variant =
  do innerType' <- resolveTypeSynonyms innerType
     case decomposeType innerType' of
       ConT innerTyCon :| ts | typename == innerTyCon ->

         let (substName, context1) = mergeArguments vars ts
             subst   = VarT <$> substName
             tyvars' = [ tv | tv <- tyvars, Map.notMember (tvName tv) subst ]

             context2 = applySubstitution subst (context1 ++ context)
             fields'  = applySubstitution subst fields
         in pure [ConstructorInfo name tyvars' context2 fields' variant
                 | name <- names]

       _ -> fail "normalizeGadtC: Expected type constructor application"

mergeArguments :: [Name] -> [Type] -> (Map Name Name, Cxt)
mergeArguments ns ts = foldl' aux (Map.empty, []) (zip ns ts)
  where
    aux (subst, context) (n,p) =
      case p of
        VarT m | Map.notMember m subst -> (Map.insert m n subst, context)
        _ -> (subst, EqualityT `AppT` VarT n `AppT` p : context)

#endif

-- | Expand all of the type synonyms in a type.
resolveTypeSynonyms :: Type -> Q Type
resolveTypeSynonyms t =
  let f :| xs = decomposeType t

      notTypeSynCase = foldl AppT f <$> traverse resolveTypeSynonyms xs

  in case f of
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
-- > t ~= foldl1 AppT (decomposeType t)
decomposeType :: Type -> NonEmpty Type
decomposeType = reverseNonEmpty . go
  where
    go (AppT f x     ) = x <| go f
#if MIN_VERSION_template_haskell(2,11,0)
    go (InfixT  l f r) = ConT f :| [l,r]
    go (UInfixT l f r) = ConT f :| [l,r]
    go (ParensT t    ) = decomposeType t
#endif
    go t               = t :| []


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
-- is determined by their order of appearance in the type singnature. (In
-- contrast with being dependent upon the Ord instance for 'Name')
--
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

-- Pred became a type synonym for Type
#if !MIN_VERSION_template_haskell(2,10,0)
instance TypeSubstitution Pred where
  freeVariables (ClassP _ xs) = freeVariables xs
  freeVariables (EqualP x y) = freeVariables x `union` freeVariables y

  applySubstitution p (ClassP n xs) = ClassP n (applySubstitution p xs)
  applySubstitution p (EqualP x y) = EqualP (applySubstitution p x)
                                            (applySubstitution p y)
#endif

-- Kind became a type synonym for Type. Previously there were no kind variables
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

-- NonEmpty didn't move into base into recently. Reimplementing it locally
-- saves dependencies for supporting older GHCs

data NonEmpty a = a :| [a]

(<|) :: a -> NonEmpty a -> NonEmpty a
x <| (y :| ys) = x :| (y : ys)

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x :| xs) = y :| ys
  where y:ys = reverse (x:xs)
