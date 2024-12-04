{-# Language CPP, DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables, TupleSections #-}

#if MIN_VERSION_template_haskell(2,12,0)
{-# Language Safe #-}
#else
{-# Language Trustworthy #-}
#endif

{-|
Module      : Language.Haskell.TH.Datatype
Description : Backwards-compatible interface to reified information about datatypes.
Copyright   : Eric Mertens 2017-2020
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a flattened view of information about data types
and newtypes that can be supported uniformly across multiple versions
of the @template-haskell@ package.

Sample output for @'reifyDatatype' ''Maybe@

@
'DatatypeInfo'
 { 'datatypeContext'   = []
 , 'datatypeName'      = GHC.Base.Maybe
 , 'datatypeVars'      = [ 'KindedTV' a_3530822107858468866 () 'StarT' ]
 , 'datatypeInstTypes' = [ 'SigT' ('VarT' a_3530822107858468866) 'StarT' ]
 , 'datatypeVariant'   = 'Datatype'
 , 'datatypeReturnKind' = 'StarT'
 , 'datatypeCons'      =
     [ 'ConstructorInfo'
         { 'constructorName'       = GHC.Base.Nothing
         , 'constructorVars'       = []
         , 'constructorContext'    = []
         , 'constructorFields'     = []
         , 'constructorStrictness' = []
         , 'constructorVariant'    = 'NormalConstructor'
         }
     , 'ConstructorInfo'
         { 'constructorName'       = GHC.Base.Just
         , 'constructorVars'       = []
         , 'constructorContext'    = []
         , 'constructorFields'     = [ 'VarT' a_3530822107858468866 ]
         , 'constructorStrictness' = [ 'FieldStrictness'
                                         'UnspecifiedUnpackedness'
                                         'Lazy'
                                     ]
         , 'constructorVariant'    = 'NormalConstructor'
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
  , FieldStrictness(..)
  , Unpackedness(..)
  , Strictness(..)

  -- * Normalization functions
  , reifyDatatype
  , reifyConstructor
  , reifyRecord
  , normalizeInfo
  , normalizeDec
  , normalizeCon

  -- * 'DatatypeInfo' lookup functions
  , lookupByConstructorName
  , lookupByRecordName

  -- * Type variable manipulation
  , TypeSubstitution(..)
  , quantifyType
  , freeVariablesWellScoped
  , freshenFreeVariables

  -- * 'Pred' functions
  , equalPred
  , classPred
  , asEqualPred
  , asClassPred

  -- * Backward compatible data definitions
  , dataDCompat
  , newtypeDCompat
  , tySynInstDCompat
  , pragLineDCompat
  , arrowKCompat

  -- * Strictness annotations
  , isStrictAnnot
  , notStrictAnnot
  , unpackedAnnot

  -- * Type simplification
  , resolveTypeSynonyms
  , resolveKindSynonyms
  , resolvePredSynonyms
  , resolveInfixT

  -- * Fixities
  , reifyFixityCompat
  , showFixity
  , showFixityDirection

  -- * Convenience functions
  , unifyTypes
  , tvName
  , tvKind
  , datatypeType
  ) where

import           Control.Monad
import           Data.Data (Data)
import           Data.Foldable (foldMap, foldl')
import           Data.List (mapAccumL, nub, find, union, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Traversable as T
import           GHC.Generics (Generic)
import           Language.Haskell.TH hiding (Extension(..))
import           Language.Haskell.TH.Datatype.Internal
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib (arrowK, starK) -- needed for th-2.4

-- | Normalized information about newtypes and data types.
--
-- 'DatatypeInfo' contains two fields, 'datatypeVars' and 'datatypeInstTypes',
-- which encode information about the argument types. The simplest explanation
-- is that 'datatypeVars' contains all the type /variables/ bound by the data
-- type constructor, while 'datatypeInstTypes' contains the type /arguments/
-- to the data type constructor. To be more precise:
--
-- * For ADTs declared with @data@ and @newtype@, it will likely be the case
--   that 'datatypeVars' and 'datatypeInstTypes' coincide. For instance, given
--   @newtype Id a = MkId a@, in the 'DatatypeInfo' for @Id@ we would
--   have @'datatypeVars' = ['KindedTV' a () 'StarT']@ and
--   @'datatypeInstVars' = ['SigT' ('VarT' a) 'StarT']@.
--
--   ADTs that leverage @PolyKinds@ may have more 'datatypeVars' than
--   'datatypeInstTypes'. For instance, given @data Proxy (a :: k) = MkProxy@,
--   in the 'DatatypeInfo' for @Proxy@ we would have
--   @'datatypeVars' = ['KindedTV' k () 'StarT', 'KindedTV' a () ('VarT' k)]@
--   (since there are two variables, @k@ and @a@), whereas
--   @'datatypeInstTypes' = ['SigT' ('VarT' a) ('VarT' k)]@, since there is
--   only one explicit type argument to @Proxy@.
--
--   The same outcome would occur if @Proxy@ were declared using
--   @TypeAbstractions@, i.e., if it were declared as
--   @data Proxy \@k (a :: k) = MkProxy@. The 'datatypeInstTypes' would /not/
--   include a separate type for @\@k@.
--
-- * For @data instance@s and @newtype instance@s of data families,
--   'datatypeVars' and 'datatypeInstTypes' can be quite different. Here is
--   an example to illustrate the difference:
--
--   @
--   data family F a b
--   data instance F (Maybe c) (f x) = MkF c (f x)
--   @
--
--   Then in the 'DatatypeInfo' for @F@'s data instance, we would have:
--
--   @
--   'datatypeVars'      = [ 'KindedTV' c () 'StarT'
--                         , 'KindedTV' f () 'StarT'
--                         , 'KindedTV' x () 'StarT' ]
--   'datatypeInstTypes' = [ 'AppT' ('ConT' ''Maybe) ('VarT' c)
--                         , 'AppT' ('VarT' f) ('VarT' x) ]
--   @
data DatatypeInfo = DatatypeInfo
  { datatypeContext   :: Cxt               -- ^ Data type context (deprecated)
  , datatypeName      :: Name              -- ^ Type constructor
  , datatypeVars      :: [TyVarBndrUnit]   -- ^ Type parameters
  , datatypeInstTypes :: [Type]            -- ^ Argument types
  , datatypeVariant   :: DatatypeVariant   -- ^ Extra information
  , datatypeReturnKind:: Kind              -- ^ Return 'Kind' of the type.
                                           --
                                           -- If normalization is unable to determine the return kind,
                                           -- then this is conservatively set to @StarT@.
  , datatypeCons      :: [ConstructorInfo] -- ^ Normalize constructor information
  }
  deriving (Show, Eq, Data, Generic)

-- | Possible variants of data type declarations.
data DatatypeVariant
  = Datatype        -- ^ Type declared with @data@ or a primitive datatype.
  | Newtype         -- ^ Type declared with @newtype@.
                    --
                    --   A 'DatatypeInfo' that uses 'Newtype' will uphold the
                    --   invariant that there will be exactly one
                    --   'ConstructorInfo' in the 'datatypeCons'.
  | DataInstance    -- ^ Type declared with @data instance@.
  | NewtypeInstance -- ^ Type declared with @newtype instance@.
                    --
                    --   A 'DatatypeInfo' that uses 'NewtypeInstance' will
                    --   uphold the invariant that there will be exactly one
                    --   'ConstructorInfo' in the 'datatypeCons'.
  | TypeData        -- ^ Type declared with @type data@.
                    --
                    --   A 'DatatypeInfo' that uses 'TypeData' will uphold the
                    --   following invariants:
                    --
                    --   * The 'datatypeContext' will be empty.
                    --
                    --   * None of the 'constructorVariant's in any of the
                    --     'datatypeCons' will be 'RecordConstructor'.
                    --
                    --   * Each of the 'constructorStrictness' values in each
                    --     of the 'datatypeCons' will be equal to
                    --     'notStrictAnnot'.
  deriving (Show, Read, Eq, Ord, Data, Generic)

-- | Normalized information about constructors associated with newtypes and
-- data types.
data ConstructorInfo = ConstructorInfo
  { constructorName       :: Name               -- ^ Constructor name
  , constructorVars       :: [TyVarBndrUnit]    -- ^ Constructor type parameters
  , constructorContext    :: Cxt                -- ^ Constructor constraints
  , constructorFields     :: [Type]             -- ^ Constructor fields
  , constructorStrictness :: [FieldStrictness]  -- ^ Constructor fields' strictness
                                                --   (Invariant: has the same length
                                                --   as constructorFields)
  , constructorVariant    :: ConstructorVariant -- ^ Extra information
  }
  deriving (Show, Eq, Data, Generic)

-- | Possible variants of data constructors.
data ConstructorVariant
  = NormalConstructor        -- ^ Constructor without field names
  | InfixConstructor         -- ^ Constructor without field names that is
                             --   declared infix
  | RecordConstructor [Name] -- ^ Constructor with field names
  deriving (Show, Eq, Ord, Data, Generic)

-- | Normalized information about a constructor field's @UNPACK@ and
-- strictness annotations.
--
-- Note that the interface for reifying strictness in Template Haskell changed
-- considerably in GHC 8.0. The presentation in this library mirrors that which
-- can be found in GHC 8.0 or later, whereas previously, unpackedness and
-- strictness were represented with a single data type:
--
-- @
-- data Strict
--   = IsStrict
--   | NotStrict
--   | Unpacked -- On GHC 7.4 or later
-- @
--
-- For backwards compatibility, we retrofit these constructors onto the
-- following three values, respectively:
--
-- @
-- 'isStrictAnnot'  = 'FieldStrictness' 'UnspecifiedUnpackedness' 'Strict'
-- 'notStrictAnnot' = 'FieldStrictness' 'UnspecifiedUnpackedness' 'UnspecifiedStrictness'
-- 'unpackedAnnot'  = 'FieldStrictness' 'Unpack' 'Strict'
-- @
data FieldStrictness = FieldStrictness
  { fieldUnpackedness :: Unpackedness
  , fieldStrictness   :: Strictness
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | Information about a constructor field's unpackedness annotation.
data Unpackedness
  = UnspecifiedUnpackedness -- ^ No annotation whatsoever
  | NoUnpack                -- ^ Annotated with @{\-\# NOUNPACK \#-\}@
  | Unpack                  -- ^ Annotated with @{\-\# UNPACK \#-\}@
  deriving (Show, Eq, Ord, Data, Generic)

-- | Information about a constructor field's strictness annotation.
data Strictness
  = UnspecifiedStrictness -- ^ No annotation whatsoever
  | Lazy                  -- ^ Annotated with @~@
  | Strict                -- ^ Annotated with @!@
  deriving (Show, Eq, Ord, Data, Generic)

isStrictAnnot, notStrictAnnot, unpackedAnnot :: FieldStrictness
isStrictAnnot  = FieldStrictness UnspecifiedUnpackedness Strict
notStrictAnnot = FieldStrictness UnspecifiedUnpackedness UnspecifiedStrictness
unpackedAnnot  = FieldStrictness Unpack Strict

-- | Construct a Type using the datatype's type constructor and type
-- parameters. Kind signatures are removed.
datatypeType :: DatatypeInfo -> Type
datatypeType di
  = foldl AppT (ConT (datatypeName di))
  $ map stripSigT
  $ datatypeInstTypes di


-- | Compute a normalized view of the metadata about a data type or newtype
-- given a constructor.
--
-- This function will accept any constructor (value or type) for a type
-- declared with newtype or data. Value constructors must be used to
-- lookup datatype information about /data instances/ and /newtype instances/,
-- as giving the type constructor of a data family is often not enough to
-- determine a particular data family instance.
--
-- In addition, this function will also accept a record selector for a
-- data type with a constructor which uses that record.
--
-- GADT constructors are normalized into datatypes with explicit equality
-- constraints. Note that no effort is made to distinguish between equalities of
-- the same (homogeneous) kind and equalities between different (heterogeneous)
-- kinds. For instance, the following GADT's constructors:
--
-- @
-- data T (a :: k -> *) where
--   MkT1 :: T Proxy
--   MkT2 :: T Maybe
-- @
--
-- will be normalized to the following equality constraints:
--
-- @
-- AppT (AppT EqualityT (VarT a)) (ConT Proxy) -- MkT1
-- AppT (AppT EqualityT (VarT a)) (ConT Maybe) -- MkT2
-- @
--
-- But only the first equality constraint is well kinded, since in the second
-- constraint, the kinds of @(a :: k -> *)@ and @(Maybe :: * -> *)@ are different.
-- Trying to categorize which constraints need homogeneous or heterogeneous
-- equality is tricky, so we leave that task to users of this library.
--
-- Primitive types (other than unboxed sums and tuples) will have
-- no @datatypeCons@ in their normalization.
--
-- This function will apply various bug-fixes to the output of the underlying
-- @template-haskell@ library in order to provide a view of datatypes in
-- as uniform a way as possible.
reifyDatatype ::
  Name {- ^ data type or constructor name -} ->
  Q DatatypeInfo
reifyDatatype n = normalizeInfo' "reifyDatatype" isReified =<< reify n

-- | Compute a normalized view of the metadata about a constructor given its
-- 'Name'. This is useful for scenarios when you don't care about the info for
-- the enclosing data type.
reifyConstructor ::
  Name {- ^ constructor name -} ->
  Q ConstructorInfo
reifyConstructor conName = do
  dataInfo <- reifyDatatype conName
  return $ lookupByConstructorName conName dataInfo

-- | Compute a normalized view of the metadata about a constructor given the
-- 'Name' of one of its record selectors. This is useful for scenarios when you
-- don't care about the info for the enclosing data type.
reifyRecord ::
  Name {- ^ record name -} ->
  Q ConstructorInfo
reifyRecord recordName = do
  dataInfo <- reifyDatatype recordName
  return $ lookupByRecordName recordName dataInfo

-- | Given a 'DatatypeInfo', find the 'ConstructorInfo' corresponding to the
-- 'Name' of one of its constructors.
lookupByConstructorName ::
  Name {- ^ constructor name -} ->
  DatatypeInfo {- ^ info for the datatype which has that constructor -} ->
  ConstructorInfo
lookupByConstructorName conName dataInfo =
  case find ((== conName) . constructorName) (datatypeCons dataInfo) of
    Just conInfo -> conInfo
    Nothing      -> error $ "Datatype " ++ nameBase (datatypeName dataInfo)
                         ++ " does not have a constructor named " ++ nameBase conName
-- | Given a 'DatatypeInfo', find the 'ConstructorInfo' corresponding to the
-- 'Name' of one of its constructors.
lookupByRecordName ::
  Name {- ^ record name -} ->
  DatatypeInfo {- ^ info for the datatype which has that constructor -} ->
  ConstructorInfo
lookupByRecordName recordName dataInfo =
  case find (conHasRecord recordName) (datatypeCons dataInfo) of
    Just conInfo -> conInfo
    Nothing      -> error $ "Datatype " ++ nameBase (datatypeName dataInfo)
                         ++ " does not have any constructors with a "
                         ++ "record selector named " ++ nameBase recordName

-- | Normalize 'Info' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
normalizeInfo :: Info -> Q DatatypeInfo
normalizeInfo = normalizeInfo' "normalizeInfo" isn'tReified

normalizeInfo' :: String -> IsReifiedDec -> Info -> Q DatatypeInfo
normalizeInfo' entry reifiedDec i =
  case i of
    (PrimTyConI name arity unlifted) -> do
#if MIN_VERSION_template_haskell(2,16,0)
      -- We provide a minimal @DataD@ because, since TH 2.16,
      -- we can rely on the call to @reifyType@ in
      -- @normalizeDecFor@ to fill in the missing details.
      normalizeDecFor reifiedDec $ DataD [] name [] Nothing [] []
#else
      -- On older versions, we are very limited in what we can deduce.
      -- All we know is the appropriate amount of type constructors.
      -- Note that this will default all kinds to @Type@, which is all
      -- that is available anyway.
      args <- replicateM arity (newName "x")
      dec <- dataDCompat (return []) name (map plainTV args) [] []
      normalizeDecFor reifiedDec dec
#endif
    ClassI{}                          -> bad "Class not supported"
    FamilyI DataFamilyD{} _           -> bad "Use a value constructor to reify a data family instance"
    FamilyI _ _                       -> bad "Type families not supported"
    TyConI dec                        -> normalizeDecFor reifiedDec dec
    DataConI name _ parent            -> reifyParent name parent
                                         -- NB: We do not pass the IsReifiedDec information here
                                         -- because there's no point. We have no choice but to
                                         -- call reify here, since we need to determine the
                                         -- parent data type/family.
    VarI recName recTy _              -> reifyRecordType recName recTy
                                         -- NB: Similarly, we do not pass the IsReifiedDec
                                         -- information here.
    _                                 -> bad "Expected a type constructor"
  where
    bad msg = fail (entry ++ ": " ++ msg)


reifyParent :: Name -> Name -> Q DatatypeInfo
reifyParent con = reifyParentWith "reifyParent" p
  where
    p :: DatatypeInfo -> Bool
    p info = con `elem` map constructorName (datatypeCons info)

reifyRecordType :: Name -> Type -> Q DatatypeInfo
reifyRecordType recName recTy =
  let (_, _, argTys :|- _) = uncurryType recTy
  in case argTys of
       dataTy:_ -> decomposeDataType dataTy
       _        -> notRecSelFailure
  where
    decomposeDataType :: Type -> Q DatatypeInfo
    decomposeDataType ty =
      do case decomposeType ty of
           ConT parent :| _ -> reifyParentWith "reifyRecordType" p parent
           _                -> notRecSelFailure

    notRecSelFailure :: Q a
    notRecSelFailure = fail $
      "reifyRecordType: Not a record selector type: " ++
      nameBase recName ++ " :: " ++ show recTy

    p :: DatatypeInfo -> Bool
    p info = any (conHasRecord recName) (datatypeCons info)

reifyParentWith ::
  String                 {- ^ prefix for error messages -} ->
  (DatatypeInfo -> Bool) {- ^ predicate for finding the right
                              data family instance -}      ->
  Name                   {- ^ parent data type name -}     ->
  Q DatatypeInfo
reifyParentWith prefix p n =
  do info <- reify n
     case info of
       TyConI dec -> normalizeDecFor isReified dec
       FamilyI dec instances ->
         do instances1 <- mapM (repairDataFam dec) instances
            instances2 <- mapM (normalizeDecFor isReified) instances1
            case find p instances2 of
              Just inst -> return inst
              Nothing   -> panic "lost the instance"
       _ -> panic "unexpected parent"
  where
    dataFamiliesOnOldGHCsError :: Q a
    dataFamiliesOnOldGHCsError = fail $
      prefix ++ ": Data family instances can only be reified with GHC 7.4 or later"

    panic :: String -> Q a
    panic message = fail $ "PANIC: " ++ prefix ++ " " ++ message

-- Sadly, Template Haskell's treatment of data family instances leaves much
-- to be desired. On all versions of GHC, TH leaves off the kind signatures on
-- the type patterns of data family instances where a kind signature isn't
-- specified explicitly. Here, we can use the parent data family's type variable
-- binders to reconstruct the kind signatures if they are missing.
repairDataFam ::
  Dec {- ^ family declaration   -} ->
  Dec {- ^ instance declaration -} ->
  Q Dec {- ^ instance declaration -}
repairDataFam famD instD
#if MIN_VERSION_template_haskell(2,15,0)
      | DataFamilyD _ dvars dk <- famD
      , NewtypeInstD cx mbInstVars nts k c deriv <- instD
      , con :| ts <- decomposeType nts
      = do ts' <- repairVarKindsWith dvars dk ts
           return $ NewtypeInstD cx mbInstVars (foldl' AppT con ts') k c deriv

      | DataFamilyD _ dvars dk <- famD
      , DataInstD cx mbInstVars nts k c deriv <- instD
      , con :| ts <- decomposeType nts
      = do ts' <- repairVarKindsWith dvars dk ts
           return $ DataInstD cx mbInstVars (foldl' AppT con ts') k c deriv
#else
      | DataFamilyD _ dvars dk <- famD
      , NewtypeInstD cx n ts k c deriv <- instD
      = do ts' <- repairVarKindsWith dvars dk ts
           return $ NewtypeInstD cx n ts' k c deriv

      | DataFamilyD _ dvars dk <- famD
      , DataInstD cx n ts k c deriv <- instD
      = do ts' <- repairVarKindsWith dvars dk ts
           return $ DataInstD cx n ts' k c deriv
#endif
repairDataFam _ instD = return instD

-- | @'repairVarKindsWith' tvbs mbKind ts@ returns @ts@, but where each element
-- has an explicit kind signature taken from a 'TyVarBndr' in the corresponding
-- position in @tvbs@, or from the corresponding kind argument in 'mbKind' if
-- there aren't enough 'TyVarBndr's available. An example where @tvbs@ can be
-- shorter than @ts@ can be found in this example from #95:
--
-- @
-- data family F :: Type -> Type
-- data instance F a = C
-- @
--
-- The @F@ has no type variable binders in its @data family@ declaration, and
-- it has a return kind of @Type -> Type@. As a result, we pair up @Type@ with
-- @VarT a@ to get @SigT a (ConT ''Type)@.
repairVarKindsWith :: [TyVarBndrVis] -> Maybe Kind -> [Type] -> Q [Type]
repairVarKindsWith tvbs mbKind ts = do
  extra_tvbs <- mkExtraKindBinders $ fromMaybe starK mbKind
  -- This list should be the same length as @ts@. If it isn't, something has
  -- gone terribly wrong.
  let tvbs' = changeTVFlags () tvbs ++ extra_tvbs
  return $ zipWith stealKindForType tvbs' ts

-- If a VarT is missing an explicit kind signature, steal it from a TyVarBndr.
stealKindForType :: TyVarBndr_ flag -> Type -> Type
stealKindForType tvb t@VarT{} = SigT t (tvKind tvb)
stealKindForType _   t        = t

-- | Normalize 'Dec' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
--
-- Beware: 'normalizeDec' can have surprising behavior when it comes to fixity.
-- For instance, if you have this quasiquoted data declaration:
--
-- @
-- [d| infix 5 :^^:
--     data Foo where
--       (:^^:) :: Int -> Int -> Foo |]
-- @
--
-- Then if you pass the 'Dec' for @Foo@ to 'normalizeDec' without splicing it
-- in a previous Template Haskell splice, then @(:^^:)@ will be labeled a 'NormalConstructor'
-- instead of an 'InfixConstructor'. This is because Template Haskell has no way to
-- reify the fixity declaration for @(:^^:)@, so it must assume there isn't one. To
-- work around this behavior, use 'reifyDatatype' instead.
normalizeDec :: Dec -> Q DatatypeInfo
normalizeDec = normalizeDecFor isn'tReified

normalizeDecFor :: IsReifiedDec -> Dec -> Q DatatypeInfo
normalizeDecFor isReified dec =
  case dec of
#if MIN_VERSION_template_haskell(2,20,0)
    TypeDataD name tyvars mbKind cons ->
      normalizeDataD [] name tyvars mbKind cons TypeData
#endif
#if MIN_VERSION_template_haskell(2,12,0)
    NewtypeD context name tyvars mbKind con _derives ->
      normalizeDataD context name tyvars mbKind [con] Newtype
    DataD context name tyvars mbKind cons _derives ->
      normalizeDataD context name tyvars mbKind cons Datatype
# if MIN_VERSION_template_haskell(2,15,0)
    NewtypeInstD context mbTyvars nameInstTys mbKind con _derives ->
      normalizeDataInstDPostTH2'15 "newtype" context mbTyvars nameInstTys
                                   mbKind [con] NewtypeInstance
    DataInstD context mbTyvars nameInstTys mbKind cons _derives ->
      normalizeDataInstDPostTH2'15 "data" context mbTyvars nameInstTys
                                   mbKind cons DataInstance
# else
    NewtypeInstD context name instTys mbKind con _derives ->
      normalizeDataInstDPreTH2'15 context name instTys mbKind [con] NewtypeInstance
    DataInstD context name instTys mbKind cons _derives ->
      normalizeDataInstDPreTH2'15 context name instTys mbKind cons DataInstance
# endif
#else
    NewtypeD context name tyvars mbKind con _derives ->
      normalizeDataD context name tyvars mbKind [con] Newtype
    DataD context name tyvars mbKind cons _derives ->
      normalizeDataD context name tyvars mbKind cons Datatype
    NewtypeInstD context name instTys mbKind con _derives ->
      normalizeDataInstDPreTH2'15 context name instTys mbKind [con] NewtypeInstance
    DataInstD context name instTys mbKind cons _derives ->
      normalizeDataInstDPreTH2'15 context name instTys mbKind cons DataInstance
#endif
    _ -> fail "normalizeDecFor: DataD or NewtypeD required"
  where
    -- We only need to repair reified declarations for data family instances.
    repair13618' :: DatatypeInfo -> Q DatatypeInfo
    repair13618' di@DatatypeInfo{datatypeVariant = variant}
      | isReified && isFamInstVariant variant
      = repair13618 di
      | otherwise
      = return di

    -- If a data type lacks an explicit return kind, use `reifyType` to compute
    -- it, as described in step (1) of Note [Tricky result kinds].
    normalizeMbKind :: Name -> [Type] -> Maybe Kind -> Q (Maybe Kind)
    normalizeMbKind _name _instTys mbKind@(Just _) = return mbKind
    normalizeMbKind name instTys Nothing = do
#if MIN_VERSION_template_haskell(2,16,0)
      mbReifiedKind <- return Nothing `recover` fmap Just (reifyType name)
      T.mapM normalizeKind mbReifiedKind
      where
        normalizeKind :: Kind -> Q Kind
        normalizeKind k = do
          k' <- resolveKindSynonyms k
          -- Step (1) in Note [Tricky result kinds]
          -- (Wrinkle: normalizeMbKind argument unification).
          let (args, res) = unravelKindUpTo instTys k'
              -- Step (2) in Note [Tricky result kinds]
              -- (Wrinkle: normalizeMbKind argument unification).
              (instTys', args') =
                unzip $
                mapMaybe
                  (\(instTy, arg) ->
                    case arg of
                      VisFADep tvb -> Just (instTy, bndrParam tvb)
                      VisFAAnon k  -> (, k) <$> sigTMaybeKind instTy)
                  args
              (subst, _) = mergeArguments args' instTys'
          -- Step (3) in Note [Tricky result kinds]
          -- (Wrinkle: normalizeMbKind argument unification).
          pure $ applySubstitution (VarT <$> subst) res
#else
      return Nothing
#endif

    -- Given a data type declaration's binders, as well as the arguments and
    -- result of its explicit return kind, compute the free type variables.
    -- For example, this:
    --
    -- @
    -- data T (a :: j) :: forall k. Maybe k -> Type
    -- @
    --
    -- Would yield:
    --
    -- @
    -- [j, (a :: j), k, (b :: k)]
    -- @
    --
    -- Where @b@ is a fresh name that is generated in 'mkExtraFunArgForalls'.
    datatypeFreeVars :: [TyVarBndr_ flag] -> FunArgs -> Kind -> [TyVarBndrUnit]
    datatypeFreeVars declBndrs kindArgs kindRes =
      freeVariablesWellScoped $
      bndrParams declBndrs ++ funArgTys kindArgs ++ [kindRes]

    normalizeDataD :: Cxt -> Name -> [TyVarBndrVis] -> Maybe Kind
                   -> [Con] -> DatatypeVariant -> Q DatatypeInfo
    normalizeDataD context name tyvars mbKind cons variant = do
      -- NB: use `filter isRequiredTvb tyvars` here. It is possible for some of
      -- the `tyvars` to be `BndrInvis` if the data type is quoted, e.g.,
      --
      --   data D @k (a :: k)
      --
      -- th-abstraction adopts the convention that all binders in the
      -- 'datatypeInstTypes' are required, so we want to filter out the `@k`.
      let tys = bndrParams $ filter isRequiredTvb tyvars
      mbKind' <- normalizeMbKind name tys mbKind
      normalize' context name tyvars tys mbKind' cons variant

    normalizeDataInstDPostTH2'15
      :: String -> Cxt -> Maybe [TyVarBndrUnit] -> Type -> Maybe Kind
      -> [Con] -> DatatypeVariant -> Q DatatypeInfo
    normalizeDataInstDPostTH2'15 what context mbTyvars nameInstTys
                                 mbKind cons variant =
      case decomposeType nameInstTys of
        ConT name :| instTys -> do
          mbKind' <- normalizeMbKind name instTys mbKind
          normalize' context name
                     (fromMaybe (freeVariablesWellScoped instTys) mbTyvars)
                     instTys mbKind' cons variant
        _ -> fail $ "Unexpected " ++ what ++ " instance head: " ++ pprint nameInstTys

    normalizeDataInstDPreTH2'15
      :: Cxt -> Name -> [Type] -> Maybe Kind
      -> [Con] -> DatatypeVariant -> Q DatatypeInfo
    normalizeDataInstDPreTH2'15 context name instTys mbKind cons variant = do
      mbKind' <- normalizeMbKind name instTys mbKind
      normalize' context name (freeVariablesWellScoped instTys)
                 instTys mbKind' cons variant

    -- The main worker of this function.
    normalize' :: Cxt -> Name -> [TyVarBndr_ flag] -> [Type] -> Maybe Kind
               -> [Con] -> DatatypeVariant -> Q DatatypeInfo
    normalize' context name tvbs instTys mbKind cons variant = do
      -- If `mbKind` is *still* Nothing after all of the work done in
      -- normalizeMbKind, then conservatively assume that the return kind is
      -- `Type`. See step (1) of Note [Tricky result kinds].
      let kind = fromMaybe starK mbKind
      kind' <- resolveKindSynonyms kind
      let (kindArgs, kindRes) = unravelType kind'
      (extra_vis_tvbs, kindArgs') <- mkExtraFunArgForalls kindArgs
      let tvbs'    = datatypeFreeVars tvbs kindArgs' kindRes
          instTys' = instTys ++ bndrParams extra_vis_tvbs
      dec <- normalizeDec' isReified context name tvbs' instTys' kindRes cons variant
      repair13618' $ giveDIVarsStarKinds isReified dec

{-
Note [Tricky result kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example, which uses UnliftedNewtypes:

  type T :: TYPE r
  newtype T where
    MkT :: forall r. Any @(TYPE r) -> T @r

This has one universally quantified type variable `r`, but making
`reifyDatatype ''T` realize this is surprisingly tricky. There root of the
trickiness is the fact that `Language.Haskell.TH.reify ''T` will yield this:

  newtype T where
    MkT :: forall r. (Any :: TYPE r) -> (T :: TYPE r)

In particular, note that:

1. `reify` does not give `T` an explicit return kind of `TYPE r`. This is bad,
   because without this, we cannot conclude that `r` is universally quantified.
2. The reified type of the `MkT` constructor uses explicit kind annotations
   instead of visible kind applications. That is, the return type is
   `T :: TYPE r` instead of `T @r`. This makes it even trickier to figure out
   that `r` is universally quantified, as `r` does not appear directly
   underneath an application of `T`.

We resolve each of these issues as follows:

1. In `normalizeDecFor.normalizeMbKind`, we attempt to use `reifyType` to look
   up the return kind of the data type. In the `T` example above, this suffices
   to conclude that `T :: TYPE r`. `reifyType` won't always work (e.g., when
   using `normalizeDec` on a data type without an explicit return kind), so for
   those situations, we conservatively assume that the data type has return kind
   `Type`.

   The implementation of `normalizeMbKind` is somewhat involved. See
   "Wrinkle: normalizeMbKind argument unification" below for more details.
2. After determining the result kind `K1`, we pass `K1` through to
   `normalizeGadtC`. In that function, we check if the return type of the data
   constructor is of the form `Ty :: K2`, and if so, we attempt to unify `K1`
   and `K2` by passing through to `mergeArguments`. In the example above, this
   lets us conclude that the `r` in the data type return kind is the same `r`
   as in the data constructor.

===================================================
== Wrinkle: normalizeMbKind argument unification ==
===================================================

Here is a slightly more involved example:

  type T2 :: TYPE r1 -> TYPE r1
  newtype T2 (a :: TYPE r2) = MkT2 a

Here, we must use `reifyType` in `normalizeMbKind` to determine that the return
kind is `TYPE r1`. But we must be careful here: `r1` is actually the same type
variable as `r2`! We don't want to accidentally end up quantifying over the two
variables separately in `datatypeInstVars`, since they're really one and the
same.

We accomplish this by doing the following:

1. After calling `reifyKind` in `normalizeMbKind`, split the result kind into
   as many arguments as there are visible binders in the data type declaration.
   In the `T2` example above, there is exactly one visible binder in
   `newtype T2 a`, so we split the kind `TYPE r1 -> TYPE r1` by one argument to
   get ([TYPE r1], TYPE r1). See `unravelKindUpTo` for how this splitting logic
   is implemented.
2. We then unify the argument kinds resuling from the splitting in the previous
   step with the corresponding kinds from the data type declaration. In the
   example above, the split argument kind is `TYPE r1`, and the binder in the
   declaration has kind `TYPE r2`, so we unify `TYPE r1` with `TYPE r2` using
   `mergeArguments` to get a substitution [r1 :-> r2].
3. We then apply the substitution from the previous step to the rest of the
   kind. In the example above, that means we apply the [r1 :-> r2] substitution
   to `TYPE r1` to obtain `TYPE r2`.

The payoff is that everything consistently refers to `r2`, rather than the mix
of `r1` and `r2` as before.
-}

-- | Create new kind variable binder names corresponding to the return kind of
-- a data type. This is useful when you have a data type like:
--
-- @
-- data Foo :: forall k. k -> Type -> Type where ...
-- @
--
-- But you want to be able to refer to the type @Foo a b@.
-- 'mkExtraKindBinders' will take the kind @forall k. k -> Type -> Type@,
-- discover that is has two visible argument kinds, and return as a result
-- two new kind variable binders @[a :: k, b :: Type]@, where @a@ and @b@
-- are fresh type variable names.
--
-- This expands kind synonyms if necessary.
mkExtraKindBinders :: Kind -> Q [TyVarBndrUnit]
mkExtraKindBinders kind = do
  kind' <- resolveKindSynonyms kind
  let (args, _) = unravelType kind'
  (extra_kvbs, _) <- mkExtraFunArgForalls args
  return extra_kvbs

-- | Take the supplied function kind arguments ('FunArgs') and do two things:
--
-- 1. For each 'FAAnon' with kind @k@, generate a fresh name @a@ and return
--    the 'TyVarBndr' @a :: k@. Also return each visible @forall@ in an
--    'FAForalls' as a 'TyVarBndr'. (This is what the list of 'TyVarBndrUnit's
--    in the return type consists of.)
--
-- 2. Return a new 'FunArgs' value where each 'FAAnon' has been replaced with
--    @'FAForalls' ('ForallVis' [a :: k])@, where @a :: k@ the corresponding
--    'TyVarBndr' computed in step (1).
--
-- As an example, consider this function kind:
--
-- @
-- forall k. k -> Type -> Type
-- @
--
-- After splitting this kind into its 'FunArgs':
--
-- @
-- ['FAForalls' ('ForallInvis' [k]), 'FAAnon' k, 'FAAnon' Type]
-- @
--
-- Calling 'mkExtraFunArgForalls' on this 'FunArgs' value would return:
--
-- @
-- ( [a :: k, b :: Type]
-- , [ 'FAForalls' ('ForallInvis' [k])
--   , 'FAForalls' ('ForallVis' [a :: k])
--   , 'FAForalls' ('ForallVis' [b :: Type])
--   ]
-- )
-- @
--
-- Where @a@ and @b@ are fresh.
--
-- This function is used in two places:
--
-- 1. As the workhorse for 'mkExtraKindBinders'.
--
-- 2. In 'normalizeDecFor', as part of computing the 'datatypeInstVars' and as
--    part of eta expanding the explicit return kind.
mkExtraFunArgForalls :: FunArgs -> Q ([TyVarBndrUnit], FunArgs)
mkExtraFunArgForalls FANil =
  return ([], FANil)
mkExtraFunArgForalls (FAForalls tele args) = do
  (extra_vis_tvbs', args') <- mkExtraFunArgForalls args
  case tele of
    ForallVis tvbs ->
      return ( tvbs ++ extra_vis_tvbs'
             , FAForalls (ForallVis tvbs) args'
             )
    ForallInvis tvbs ->
      return ( extra_vis_tvbs'
             , FAForalls (ForallInvis tvbs) args'
             )
mkExtraFunArgForalls (FACxt ctxt args) = do
  (extra_vis_tvbs', args') <- mkExtraFunArgForalls args
  return (extra_vis_tvbs', FACxt ctxt args')
mkExtraFunArgForalls (FAAnon anon args) = do
  name <- newName "x"
  let tvb = kindedTV name anon
  (extra_vis_tvbs', args') <- mkExtraFunArgForalls args
  return ( tvb : extra_vis_tvbs'
         , FAForalls (ForallVis [tvb]) args'
         )

-- | Is a declaration for a @data instance@ or @newtype instance@?
isFamInstVariant :: DatatypeVariant -> Bool
isFamInstVariant dv =
  case dv of
    Datatype        -> False
    Newtype         -> False
    DataInstance    -> True
    NewtypeInstance -> True
    TypeData        -> False

bndrParams :: [TyVarBndr_ flag] -> [Type]
bndrParams = map bndrParam

bndrParam :: TyVarBndr_ flag -> Type
bndrParam = elimTV VarT (\n k -> SigT (VarT n) k)

-- | Returns 'True' if the flag of the supplied 'TyVarBndrVis' is 'BndrReq'.
isRequiredTvb :: TyVarBndrVis -> Bool
isRequiredTvb tvb = tvFlag tvb == BndrReq

-- | Remove the outermost 'SigT'.
stripSigT :: Type -> Type
stripSigT (SigT t _) = t
stripSigT t          = t

-- | If the supplied 'Type' is a @'SigT' _ k@, return @'Just' k@. Otherwise,
-- return 'Nothing'.
sigTMaybeKind :: Type -> Maybe Kind
sigTMaybeKind (SigT _ k) = Just k
sigTMaybeKind _          = Nothing

normalizeDec' ::
  IsReifiedDec    {- ^ Is this a reified 'Dec'? -} ->
  Cxt             {- ^ Datatype context         -} ->
  Name            {- ^ Type constructor         -} ->
  [TyVarBndrUnit] {- ^ Type parameters          -} ->
  [Type]          {- ^ Argument types           -} ->
  Kind            {- ^ Result kind              -} ->
  [Con]           {- ^ Constructors             -} ->
  DatatypeVariant {- ^ Extra information        -} ->
  Q DatatypeInfo
normalizeDec' reifiedDec context name params instTys resKind cons variant =
  do cons' <- concat <$> mapM (normalizeConFor reifiedDec name params instTys resKind variant) cons
     return DatatypeInfo
       { datatypeContext   = context
       , datatypeName      = name
       , datatypeVars      = params
       , datatypeInstTypes = instTys
       , datatypeCons      = cons'
       , datatypeReturnKind = resKind
       , datatypeVariant   = variant
       }

-- | Normalize a 'Con' into a 'ConstructorInfo'. This requires knowledge of
-- the type and parameters of the constructor, as well as whether the constructor
-- is for a data family instance, as extracted from the outer
-- 'Dec'.
normalizeCon ::
  Name            {- ^ Type constructor  -} ->
  [TyVarBndrUnit] {- ^ Type parameters   -} ->
  [Type]          {- ^ Argument types    -} ->
  Kind            {- ^ Result kind       -} ->
  DatatypeVariant {- ^ Extra information -} ->
  Con             {- ^ Constructor       -} ->
  Q [ConstructorInfo]
normalizeCon = normalizeConFor isn'tReified

normalizeConFor ::
  IsReifiedDec    {- ^ Is this a reified 'Dec'? -} ->
  Name            {- ^ Type constructor         -} ->
  [TyVarBndrUnit] {- ^ Type parameters          -} ->
  [Type]          {- ^ Argument types           -} ->
  Kind            {- ^ Result kind              -} ->
  DatatypeVariant {- ^ Extra information        -} ->
  Con             {- ^ Constructor              -} ->
  Q [ConstructorInfo]
normalizeConFor reifiedDec typename params instTys resKind variant =
  fmap (map (giveCIVarsStarKinds reifiedDec)) . dispatch
  where
    -- A GADT constructor is declared infix when:
    --
    -- 1. Its name uses operator syntax (e.g., (:*:))
    -- 2. It has exactly two fields
    -- 3. It has a programmer-supplied fixity declaration
    checkGadtFixity :: [Type] -> Name -> Q ConstructorVariant
    checkGadtFixity ts n = do
      -- Don't call reifyFixityCompat here! We need to be able to distinguish
      -- between a default fixity and an explicit @infixl 9@.
      mbFi <- return Nothing `recover` reifyFixity n
      let userSuppliedFixity = isJust mbFi
      return $ if isInfixDataCon (nameBase n)
                  && length ts == 2
                  && userSuppliedFixity
               then InfixConstructor
               else NormalConstructor

    -- Checks if a String names a valid Haskell infix data
    -- constructor (i.e., does it begin with a colon?).
    isInfixDataCon :: String -> Bool
    isInfixDataCon (':':_) = True
    isInfixDataCon _       = False

    dispatch :: Con -> Q [ConstructorInfo]
    dispatch =
      let defaultCase :: Con -> Q [ConstructorInfo]
          defaultCase = go [] [] False
            where
              go :: [TyVarBndrUnit]
                 -> Cxt
                 -> Bool -- Is this a GADT? (see the documentation for
                         -- for checkGadtFixity)
                 -> Con
                 -> Q [ConstructorInfo]
              go tyvars context gadt c =
                case c of
                  NormalC n xs -> do
                    let (bangs, ts) = unzip xs
                        stricts     = map normalizeStrictness bangs
                    fi <- if gadt
                             then checkGadtFixity ts n
                             else return NormalConstructor
                    return [ConstructorInfo n tyvars context ts stricts fi]
                  InfixC l n r ->
                    let (bangs, ts) = unzip [l,r]
                        stricts     = map normalizeStrictness bangs in
                    return [ConstructorInfo n tyvars context ts stricts
                                            InfixConstructor]
                  RecC n xs ->
                    let fns     = takeFieldNames xs
                        stricts = takeFieldStrictness xs in
                    return [ConstructorInfo n tyvars context
                              (takeFieldTypes xs) stricts (RecordConstructor fns)]
                  ForallC tyvars' context' c' ->
                    go (changeTVFlags () tyvars'++tyvars) (context'++context) True c'
                  GadtC ns xs innerType ->
                    let (bangs, ts) = unzip xs
                        stricts     = map normalizeStrictness bangs in
                    gadtCase ns innerType ts stricts (checkGadtFixity ts)
                  RecGadtC ns xs innerType ->
                    let fns     = takeFieldNames xs
                        stricts = takeFieldStrictness xs in
                    gadtCase ns innerType (takeFieldTypes xs) stricts
                             (const $ return $ RecordConstructor fns)
                where
                  gadtCase = normalizeGadtC typename params instTys resKind tyvars context
      in defaultCase

normalizeStrictness :: Bang -> FieldStrictness
normalizeStrictness (Bang upk str) =
  FieldStrictness (normalizeSourceUnpackedness upk)
                  (normalizeSourceStrictness str)
  where
    normalizeSourceUnpackedness :: SourceUnpackedness -> Unpackedness
    normalizeSourceUnpackedness NoSourceUnpackedness = UnspecifiedUnpackedness
    normalizeSourceUnpackedness SourceNoUnpack       = NoUnpack
    normalizeSourceUnpackedness SourceUnpack         = Unpack

    normalizeSourceStrictness :: SourceStrictness -> Strictness
    normalizeSourceStrictness NoSourceStrictness = UnspecifiedStrictness
    normalizeSourceStrictness SourceLazy         = Lazy
    normalizeSourceStrictness SourceStrict       = Strict

normalizeGadtC ::
  Name              {- ^ Type constructor             -} ->
  [TyVarBndrUnit]   {- ^ Type parameters              -} ->
  [Type]            {- ^ Argument types               -} ->
  Kind              {- ^ Result kind                  -} ->
  [TyVarBndrUnit]   {- ^ Constructor parameters       -} ->
  Cxt               {- ^ Constructor context          -} ->
  [Name]            {- ^ Constructor names            -} ->
  Type              {- ^ Declared type of constructor -} ->
  [Type]            {- ^ Constructor field types      -} ->
  [FieldStrictness] {- ^ Constructor field strictness -} ->
  (Name -> Q ConstructorVariant)
                    {- ^ Determine a constructor variant
                         from its 'Name' -}              ->
  Q [ConstructorInfo]
normalizeGadtC typename params instTys resKind tyvars context names innerType
               fields stricts getVariant =
  do -- It's possible that the constructor has implicitly quantified type
     -- variables, such as in the following example (from #58):
     --
     --   [d| data Foo where
     --         MkFoo :: a -> Foo |]
     --
     -- normalizeGadtC assumes that all type variables have binders, however,
     -- so we use freeVariablesWellScoped to obtain the implicit type
     -- variables' binders before proceeding.
     let implicitTyvars = freeVariablesWellScoped
                          [curryType (changeTVFlags SpecifiedSpec tyvars)
                                     context fields innerType]
         allTyvars = implicitTyvars ++ tyvars

     -- Due to GHC Trac #13885, it's possible that the type variables bound by
     -- a GADT constructor will shadow those that are bound by the data type.
     -- This function assumes this isn't the case in certain parts (e.g., when
     -- mergeArguments is invoked), so we do an alpha-renaming of the
     -- constructor-bound variables before proceeding. See #36 for an example
     -- of what can go wrong if this isn't done.
     let conBoundNames =
           concatMap (\tvb -> tvName tvb:freeVariables (tvKind tvb)) allTyvars
     conSubst <- T.sequence $ Map.fromList [ (n, newName (nameBase n))
                                           | n <- conBoundNames ]
     let conSubst'     = fmap VarT conSubst
         renamedTyvars =
           map (elimTV (\n   -> plainTV  (conSubst Map.! n))
                       (\n k -> kindedTV (conSubst Map.! n)
                                         (applySubstitution conSubst' k))) allTyvars
         renamedContext   = applySubstitution conSubst' context
         renamedInnerType = applySubstitution conSubst' innerType
         renamedFields    = applySubstitution conSubst' fields

     innerType' <- resolveTypeSynonyms renamedInnerType

     -- If the return type in the data constructor is of the form `T :: K`, then
     -- return (T, Just K, Just resKind), where `resKind` is the result kind of
     -- the parent data type. Otherwise, return (T :: K, Nothing, Nothing). The
     -- two `Maybe` values are passed below to `mergeArguments` such that if
     -- they are both `Just`, then we will attempt to unify `K` and `resKind`.
     -- See step (2) of Note [Tricky result kinds].
     let (innerType'', mbInnerResKind, mbResKind) =
           case innerType' of
             SigT t innerResKind -> (t, Just innerResKind, Just resKind)
             _                   -> (innerType', Nothing, Nothing)

     case decomposeType innerType'' of
       ConT innerTyCon :| ts | typename == innerTyCon ->

         let -- See step (2) of Note [Tricky result kinds].
             instTys' = maybeToList mbResKind ++ instTys
             ts' = maybeToList mbInnerResKind ++ ts

             (substName, context1) =
               closeOverKinds (kindsOfFVsOfTvbs renamedTyvars)
                              (kindsOfFVsOfTvbs params)
                              (mergeArguments instTys' ts')
             subst    = VarT <$> substName
             exTyvars = [ tv | tv <- renamedTyvars, Map.notMember (tvName tv) subst ]

             -- The use of substTyVarBndrKinds below will never capture, as the
             -- range of the substitution will always use distinct names from
             -- exTyvars due to the alpha-renaming pass above.
             exTyvars' = substTyVarBndrKinds subst exTyvars
             context2  = applySubstitution   subst (context1 ++ renamedContext)
             fields'   = applySubstitution   subst renamedFields
         in sequence [ ConstructorInfo name exTyvars' context2
                                       fields' stricts <$> variantQ
                     | name <- names
                     , let variantQ = getVariant name
                     ]

       _ -> fail "normalizeGadtC: Expected type constructor application"

{-
Extend a type variable renaming subtitution and a list of equality
predicates by looking into kind information as much as possible.

Why is this necessary? Consider the following example:

  data (a1 :: k1) :~: (b1 :: k1) where
    Refl :: forall k2 (a2 :: k2). a2 :~: a2

After an initial call to mergeArguments, we will have the following
substitution and context:

* Substitution: [a2 :-> a1]
* Context: (a2 ~ b1)

We shouldn't stop there, however! We determine the existentially quantified
type variables of a constructor by filtering out those constructor-bound
variables which do not appear in the substitution that mergeArguments
returns. In this example, Refl's bound variables are k2 and a2. a2 appears
in the returned substitution, but k2 does not, which means that we would
mistakenly conclude that k2 is existential!

Although we don't have the full power of kind inference to guide us here, we
can at least do the next best thing. Generally, the datatype-bound type
variables and the constructor type variable binders contain all of the kind
information we need, so we proceed as follows:

1. Construct a map from each constructor-bound variable to its kind. (Do the
   same for each datatype-bound variable). These maps are the first and second
   arguments to closeOverKinds, respectively.
2. Call mergeArguments once on the GADT return type and datatype-bound types,
   and pass that in as the third argument to closeOverKinds.
3. For each name-name pair in the supplied substitution, check if the first and
   second names map to kinds in the first and second kind maps in
   closeOverKinds, respectively. If so, associate the first kind with the
   second kind.
4. For each kind association discovered in part (3), call mergeArguments
   on the lists of kinds. This will yield a kind substitution and kind
   equality context.
5. If the kind substitution is non-empty, then go back to step (3) and repeat
   the process on the new kind substitution and context.

   Otherwise, if the kind substitution is empty, then we have reached a fixed-
   point (i.e., we have closed over the kinds), so proceed.
6. Union up all of the substitutions and contexts, and return those.

This algorithm is not perfect, as it will only catch everything if all of
the kinds are explicitly mentioned somewhere (and not left quantified
implicitly). Thankfully, reifying data types via Template Haskell tends to
yield a healthy amount of kind signatures, so this works quite well in
practice.
-}
closeOverKinds :: Map Name Kind
               -> Map Name Kind
               -> (Map Name Name, Cxt)
               -> (Map Name Name, Cxt)
closeOverKinds domainFVKinds rangeFVKinds = go
  where
    go :: (Map Name Name, Cxt) -> (Map Name Name, Cxt)
    go (subst, context) =
      let substList = Map.toList subst
          (kindsInner, kindsOuter) =
            unzip $
            mapMaybe (\(d, r) -> do d' <- Map.lookup d domainFVKinds
                                    r' <- Map.lookup r rangeFVKinds
                                    return (d', r'))
                     substList
          (kindSubst, kindContext) = mergeArguments kindsOuter kindsInner
          (restSubst, restContext)
            = if Map.null kindSubst -- Fixed-point calculation
                 then (Map.empty, [])
                 else go (kindSubst, kindContext)
          finalSubst   = Map.unions [subst, kindSubst, restSubst]
          finalContext = nub $ concat [context, kindContext, restContext]
            -- Use `nub` here in an effort to minimize the number of
            -- redundant equality constraints in the returned context.
      in (finalSubst, finalContext)

-- Look into a list of types and map each free variable name to its kind.
kindsOfFVsOfTypes :: [Type] -> Map Name Kind
kindsOfFVsOfTypes = foldMap go
  where
    go :: Type -> Map Name Kind
    go (AppT t1 t2) = go t1 `Map.union` go t2
    go (SigT t k) =
      let kSigs = go k
      in case t of
           VarT n -> Map.insert n k kSigs
           _      -> go t `Map.union` kSigs

    go (ForallT {})    = forallError
#if MIN_VERSION_template_haskell(2,16,0)
    go (ForallVisT {}) = forallError
#endif

    go _ = Map.empty

    forallError :: a
    forallError = error "`forall` type used in data family pattern"

-- Look into a list of type variable binder and map each free variable name
-- to its kind (also map the names that KindedTVs bind to their respective
-- kinds). This function considers the kind of a PlainTV to be *.
kindsOfFVsOfTvbs :: [TyVarBndr_ flag] -> Map Name Kind
kindsOfFVsOfTvbs = foldMap go
  where
    go :: TyVarBndr_ flag -> Map Name Kind
    go = elimTV (\n -> Map.singleton n starK)
                (\n k -> let kSigs = kindsOfFVsOfTypes [k]
                         in Map.insert n k kSigs)

mergeArguments ::
  [Type] {- ^ outer parameters                    -} ->
  [Type] {- ^ inner parameters (specializations ) -} ->
  (Map Name Name, Cxt)
mergeArguments ns ts = foldr aux (Map.empty, []) (zip ns ts)
  where

    aux (f `AppT` x, g `AppT` y) sc =
      aux (x,y) (aux (f,g) sc)

    aux (VarT n,p) (subst, context) =
      case p of
        VarT m | m == n  -> (subst, context)
                   -- If the two variables are the same, don't bother extending
                   -- the substitution. (This is purely an optimization.)
               | Just n' <- Map.lookup m subst
               , n == n' -> (subst, context)
                   -- If a variable is already in a substitution and it maps
                   -- to the variable that we are trying to unify with, then
                   -- leave the context alone. (Not doing so caused #46.)
               | Map.notMember m subst -> (Map.insert m n subst, context)
        _ -> (subst, equalPred (VarT n) p : context)

    aux (SigT x _, y) sc = aux (x,y) sc -- learn about kinds??
    -- This matches *after* VarT so that we can compute a substitution
    -- that includes the kind signature.
    aux (x, SigT y _) sc = aux (x,y) sc

    aux _ sc = sc

-- | Expand all of the type synonyms in a type.
--
-- Note that this function will drop parentheses as a side effect.
resolveTypeSynonyms :: Type -> Q Type
resolveTypeSynonyms t =
  let (f, xs) = decomposeTypeArgs t
      normal_xs = filterTANormals xs

      -- Either the type is not headed by a type synonym, or it is headed by a
      -- type synonym that is not applied to enough arguments. Leave the type
      -- alone and only expand its arguments.
      defaultCase :: Type -> Q Type
      defaultCase ty = foldl appTypeArg ty <$> mapM resolveTypeArgSynonyms xs

      expandCon :: Name -- The Name to check whether it is a type synonym or not
                -> Type -- The argument type to fall back on if the supplied
                        -- Name isn't a type synonym
                -> Q Type
      expandCon n ty = do
        mbInfo <- reifyMaybe n
        case mbInfo of
          Just (TyConI (TySynD _ synvars def))
            |  length normal_xs >= length synvars -- Don't expand undersaturated type synonyms (#88)
            -> resolveTypeSynonyms $ expandSynonymRHS synvars normal_xs def
          _ -> defaultCase ty

  in case f of
       ForallT tvbs ctxt body ->
         ForallT `fmap` mapM resolve_tvb_syns tvbs
                   `ap` mapM resolvePredSynonyms ctxt
                   `ap` resolveTypeSynonyms body
       SigT ty ki -> do
         ty' <- resolveTypeSynonyms ty
         ki' <- resolveKindSynonyms ki
         defaultCase $ SigT ty' ki'
       ConT n -> expandCon n f
       InfixT t1 n t2 -> do
         t1' <- resolveTypeSynonyms t1
         t2' <- resolveTypeSynonyms t2
         expandCon n (InfixT t1' n t2')
       UInfixT t1 n t2 -> do
         t1' <- resolveTypeSynonyms t1
         t2' <- resolveTypeSynonyms t2
         expandCon n (UInfixT t1' n t2')
#if MIN_VERSION_template_haskell(2,15,0)
       ImplicitParamT n t -> do
         ImplicitParamT n <$> resolveTypeSynonyms t
#endif
#if MIN_VERSION_template_haskell(2,16,0)
       ForallVisT tvbs body ->
         ForallVisT `fmap` mapM resolve_tvb_syns tvbs
                      `ap` resolveTypeSynonyms body
#endif
#if MIN_VERSION_template_haskell(2,19,0)
       PromotedInfixT t1 n t2 -> do
         t1' <- resolveTypeSynonyms t1
         t2' <- resolveTypeSynonyms t2
         return $ PromotedInfixT t1' n t2'
       PromotedUInfixT t1 n t2 -> do
         t1' <- resolveTypeSynonyms t1
         t2' <- resolveTypeSynonyms t2
         return $ PromotedUInfixT t1' n t2'
#endif
       _ -> defaultCase f

-- | Expand all of the type synonyms in a 'TypeArg'.
resolveTypeArgSynonyms :: TypeArg -> Q TypeArg
resolveTypeArgSynonyms (TANormal t) = TANormal <$> resolveTypeSynonyms t
resolveTypeArgSynonyms (TyArg k)    = TyArg    <$> resolveKindSynonyms k

-- | Expand all of the type synonyms in a 'Kind'.
resolveKindSynonyms :: Kind -> Q Kind
resolveKindSynonyms = resolveTypeSynonyms

-- | Expand all of the type synonyms in a the kind of a 'TyVarBndr'.
resolve_tvb_syns :: TyVarBndr_ flag -> Q (TyVarBndr_ flag)
resolve_tvb_syns = mapMTVKind resolveKindSynonyms

expandSynonymRHS ::
  [TyVarBndr_ flag] {- ^ Substitute these variables... -} ->
  [Type]            {- ^ ...with these types... -} ->
  Type              {- ^ ...inside of this type. -} ->
  Type
expandSynonymRHS synvars ts def =
  let argNames    = map tvName synvars
      (args,rest) = splitAt (length argNames) ts
      subst       = Map.fromList (zip argNames args)
  in foldl AppT (applySubstitution subst def) rest

-- | Expand all of the type synonyms in a 'Pred'.
resolvePredSynonyms :: Pred -> Q Pred
resolvePredSynonyms = resolveTypeSynonyms

-- | Decompose a type into a list of it's outermost applications. This process
-- forgets about infix application, explicit parentheses, and visible kind
-- applications.
--
-- This operation should be used after all 'UInfixT' cases have been resolved
-- by 'resolveFixities' if the argument is being user generated.
--
-- > t ~= foldl1 AppT (decomposeType t)
decomposeType :: Type -> NonEmpty Type
decomposeType t =
  case decomposeTypeArgs t of
    (f, x) -> f :| filterTANormals x

-- | A variant of 'decomposeType' that preserves information about visible kind
-- applications by returning a 'NonEmpty' list of 'TypeArg's.
decomposeTypeArgs :: Type -> (Type, [TypeArg])
decomposeTypeArgs = go []
  where
    go :: [TypeArg] -> Type -> (Type, [TypeArg])
    go args (AppT f x)     = go (TANormal x:args) f
    go args (ParensT t)    = go args t
#if MIN_VERSION_template_haskell(2,15,0)
    go args (AppKindT f x) = go (TyArg x:args) f
#endif
    go args t              = (t, args)

-- | An argument to a type, either a normal type ('TANormal') or a visible
-- kind application ('TyArg').
data TypeArg
  = TANormal Type
  | TyArg Kind

-- | Apply a 'Type' to a 'TypeArg'.
appTypeArg :: Type -> TypeArg -> Type
appTypeArg f (TANormal x) = f `AppT` x
appTypeArg f (TyArg _k) =
#if MIN_VERSION_template_haskell(2,15,0)
  f `AppKindT` _k
#else
  f -- VKA isn't supported, so conservatively drop the argument
#endif

-- | Filter out all of the normal type arguments from a list of 'TypeArg's.
filterTANormals :: [TypeArg] -> [Type]
filterTANormals = mapMaybe f
  where
    f :: TypeArg -> Maybe Type
    f (TANormal t) = Just t
    f (TyArg {})   = Nothing

-- 'NonEmpty' didn't move into base until recently. Reimplementing it locally
-- saves dependencies for supporting older GHCs
data NonEmpty a = a :| [a]

data NonEmptySnoc a = [a] :|- a

-- Decompose a function type into its context, argument types,
-- and return type. For instance, this
--
--   forall a b. (Show a, b ~ Int) => (a -> b) -> Char -> Int
--
-- becomes
--
--   ([a, b], [Show a, b ~ Int], [a -> b, Char] :|- Int)
uncurryType :: Type -> ([TyVarBndrSpec], Cxt, NonEmptySnoc Type)
uncurryType = go [] [] []
  where
    go tvbs ctxt args (AppT (AppT ArrowT t1) t2) = go tvbs ctxt (t1:args) t2
    go tvbs ctxt args (ForallT tvbs' ctxt' t)    = go (tvbs++tvbs') (ctxt++ctxt') args t
    go tvbs ctxt args t                          = (tvbs, ctxt, reverse args :|- t)

-- Reconstruct a function type from its type variable binders, context,
-- argument types and return type.
curryType :: [TyVarBndrSpec] -> Cxt -> [Type] -> Type -> Type
curryType tvbs ctxt args res =
  ForallT tvbs ctxt $ foldr (\arg t -> ArrowT `AppT` arg `AppT` t) res args

-- All of the code from @ForallTelescope@ through @unravelType@ is taken from
-- the @th-desugar@ library, which is licensed under a 3-Clause BSD license.

-- | The type variable binders in a @forall@. This is not used by the TH AST
-- itself, but this is used as an intermediate data type in 'FAForalls'.
data ForallTelescope
  = ForallVis [TyVarBndrUnit]
    -- ^ A visible @forall@ (e.g., @forall a -> {...}@).
    --   These do not have any notion of specificity, so we use
    --   '()' as a placeholder value in the 'TyVarBndr's.
  | ForallInvis [TyVarBndrSpec]
    -- ^ An invisible @forall@ (e.g., @forall a {b} c -> {...}@),
    --   where each binder has a 'Specificity'.

-- | The list of arguments in a function 'Type'.
data FunArgs
  = FANil
    -- ^ No more arguments.
  | FAForalls ForallTelescope FunArgs
    -- ^ A series of @forall@ed type variables followed by a dot (if
    --   'ForallInvis') or an arrow (if 'ForallVis'). For example,
    --   the type variables @a1 ... an@ in @forall a1 ... an. r@.
  | FACxt Cxt FunArgs
    -- ^ A series of constraint arguments followed by @=>@. For example,
    --   the @(c1, ..., cn)@ in @(c1, ..., cn) => r@.
  | FAAnon Kind FunArgs
    -- ^ An anonymous argument followed by an arrow. For example, the @a@
    --   in @a -> r@.

-- | A /visible/ function argument type (i.e., one that must be supplied
-- explicitly in the source code). This is in contrast to /invisible/
-- arguments (e.g., the @c@ in @c => r@), which are instantiated without
-- the need for explicit user input.
data VisFunArg
  = VisFADep TyVarBndrUnit
    -- ^ A visible @forall@ (e.g., @forall a -> a@).
  | VisFAAnon Kind
    -- ^ An anonymous argument followed by an arrow (e.g., @a -> r@).

-- | Decompose a function 'Type' into its arguments (the 'FunArgs') and its
-- result type (the 'Type).
unravelType :: Type -> (FunArgs, Type)
unravelType (ForallT tvbs cxt ty) =
  let (args, res) = unravelType ty in
  (FAForalls (ForallInvis tvbs) (FACxt cxt args), res)
unravelType (AppT (AppT ArrowT t1) t2) =
  let (args, res) = unravelType t2 in
  (FAAnon t1 args, res)
#if __GLASGOW_HASKELL__ >= 809
unravelType (ForallVisT tvbs ty) =
  let (args, res) = unravelType ty in
  (FAForalls (ForallVis tvbs) args, res)
#endif
unravelType t = (FANil, t)

-- | Reconstruct an arrow 'Type' from its argument and result types.
ravelType :: FunArgs -> Type -> Type
ravelType FANil res = res
-- We need a special case for FAForalls ForallInvis followed by FACxt so that we may
-- collapse them into a single ForallT when raveling.
ravelType (FAForalls (ForallInvis tvbs) (FACxt p args)) res =
  ForallT tvbs p (ravelType args res)
ravelType (FAForalls (ForallInvis  tvbs)  args)  res = ForallT tvbs [] (ravelType args res)
ravelType (FAForalls (ForallVis   _tvbs) _args) _res =
#if __GLASGOW_HASKELL__ >= 809
      ForallVisT _tvbs (ravelType _args _res)
#else
      error "Visible dependent quantification supported only on GHC 8.10+"
#endif
ravelType (FACxt cxt args) res = ForallT [] cxt (ravelType args res)
ravelType (FAAnon t args)  res = AppT (AppT ArrowT t) (ravelType args res)

-- | Convert a 'FunArg's value into the list of 'Type's that it contains.
-- For example, given this function type:
--
-- @
-- forall k (a :: k). Proxy a -> forall b. Maybe b
-- @
--
-- Then calling @funArgTys@ on the arguments would yield:
--
-- @
-- [k, (a :: k), Proxy a, b, Maybe b]
-- @
--
-- This is primarily used for the purposes of computing all of the type
-- variables that appear in a 'FunArgs' value.
funArgTys :: FunArgs -> [Type]
funArgTys FANil = []
funArgTys (FAForalls tele args) =
  forallTelescopeTys tele ++ funArgTys args
funArgTys (FACxt ctxt args) =
  ctxt ++ funArgTys args
funArgTys (FAAnon anon args) =
  anon : funArgTys args

-- | Convert a 'ForallTelescope' value into the list of 'Type's that it
-- contains. See the Haddocks for 'funArgTys' for an example of what this does.
forallTelescopeTys :: ForallTelescope -> [Type]
forallTelescopeTys (ForallVis tvbs)   = bndrParams tvbs
forallTelescopeTys (ForallInvis tvbs) = bndrParams tvbs

-- | @'filterVisFunArgsUpTo' xs args@ will split @args@ into 'VisFunArg's as
-- many times as there are elements in @xs@, pairing up each entry in @xs@ with
-- the corresponding 'VisFunArg' in the process. This will stop after the last
-- entry in @xs@ has been paired up.
--
-- For example, this:
--
-- @
-- 'filterVisFunArgsUpTo'
--   [Bool, True]
--   [ FAForalls (ForallVis [j])
--   , FAAnon j
--   , FAForalls (ForallInvis [k])
--   , FAAnon k
--   ]
-- @
--
-- Will yield:
--
-- @
-- ( [(Bool, VisFADep j), (True, VisFAAnon j)]
-- , [FAForalls (ForallInvis [k]), FAAnon k]
-- )
-- @
--
-- This function assumes the precondition that there are at least as many
-- visible function arguments in @args@ as there are elements in @xs@. If this
-- is not the case, this function will raise an error.
filterVisFunArgsUpTo :: forall a. [a] -> FunArgs -> ([(a, VisFunArg)], FunArgs)
filterVisFunArgsUpTo = go_fun_args
  where
    go_fun_args :: [a] -> FunArgs -> ([(a, VisFunArg)], FunArgs)
    go_fun_args [] args =
      ([], args)
    go_fun_args (_:_) FANil =
      error "filterVisFunArgsUpTo.go_fun_args: Too few FunArgs"
    go_fun_args xs (FACxt _ args) =
      go_fun_args xs args
    go_fun_args (x:xs) (FAAnon t args) =
      let (xs', args') = go_fun_args xs args in
      ((x, VisFAAnon t):xs', args')
    go_fun_args xs (FAForalls tele args) =
      case tele of
        ForallVis tvbs ->
          go_vis_tvbs tvbs xs args
        ForallInvis _ ->
          go_fun_args xs args

    go_vis_tvbs :: [TyVarBndrUnit] -> [a] -> FunArgs -> ([(a, VisFunArg)], FunArgs)
    go_vis_tvbs [] xs args =
      go_fun_args xs args
    go_vis_tvbs (tvb:tvbs) (x:xs) args =
      let (xs', args') = go_vis_tvbs tvbs xs args in
      ((x, VisFADep tvb):xs', args')
    go_vis_tvbs tvbs [] args =
      ([], FAForalls (ForallVis tvbs) args)

-- | @'unravelKindUpTo' xs k@ will split the function kind @k@ into its argument
-- kinds @args@ and result kind @res@, and then it will call
-- @'filterVisFunArgsUpTo' xs args@. The leftover arguments that were not split
-- apart by 'filterVisFunArgsUpTo' are then raveled back into @res@.
--
-- For example, this:
--
-- @
-- 'filterVisFunArgsUpTo'
--   [Bool, True]
--   (forall j -> j -> forall k. k -> Type)
-- @
--
-- Will yield:
--
-- @
-- ( [(Bool, VisFADep j), (True, VisFAAnon j)]
-- , forall k. k -> Type
-- )
-- @
--
-- This function assumes the precondition that there are at least as many
-- visible function arguments in @args@ as there are elements in @xs@. If this
-- is not the case, this function will raise an error.
unravelKindUpTo :: [a] -> Kind -> ([(a, VisFunArg)], Kind)
unravelKindUpTo xs k = (xs', ravelType args' res)
  where
    (args, res) = unravelType k
    (xs', args') = filterVisFunArgsUpTo xs args

-- | Resolve any infix type application in a type using the fixities that
-- are currently available. Starting in `template-haskell-2.11` types could
-- contain unresolved infix applications.
resolveInfixT :: Type -> Q Type

resolveInfixT (ForallT vs cx t) = ForallT <$> traverse (traverseTVKind resolveInfixT) vs
                                          <*> mapM resolveInfixT cx
                                          <*> resolveInfixT t
resolveInfixT (f `AppT` x)      = resolveInfixT f `appT` resolveInfixT x
resolveInfixT (ParensT t)       = resolveInfixT t
resolveInfixT (InfixT l o r)    = conT o `appT` resolveInfixT l `appT` resolveInfixT r
resolveInfixT (SigT t k)        = SigT <$> resolveInfixT t <*> resolveInfixT k
resolveInfixT t@UInfixT{}       = resolveInfixT =<< resolveInfixT1 (gatherUInfixT t)
#if MIN_VERSION_template_haskell(2,15,0)
resolveInfixT (f `AppKindT` x)  = appKindT (resolveInfixT f) (resolveInfixT x)
resolveInfixT (ImplicitParamT n t)
                                = implicitParamT n $ resolveInfixT t
#endif
#if MIN_VERSION_template_haskell(2,16,0)
resolveInfixT (ForallVisT vs t) = ForallVisT <$> traverse (traverseTVKind resolveInfixT) vs
                                             <*> resolveInfixT t
#endif
#if MIN_VERSION_template_haskell(2,19,0)
resolveInfixT (PromotedInfixT l o r)
                                = promotedT o `appT` resolveInfixT l `appT` resolveInfixT r
resolveInfixT t@PromotedUInfixT{}
                                = resolveInfixT =<< resolveInfixT1 (gatherUInfixT t)
#endif
resolveInfixT t                 = return t

gatherUInfixT :: Type -> InfixList
gatherUInfixT (UInfixT l o r)         = ilAppend (gatherUInfixT l) o False (gatherUInfixT r)
#if MIN_VERSION_template_haskell(2,19,0)
gatherUInfixT (PromotedUInfixT l o r) = ilAppend (gatherUInfixT l) o True  (gatherUInfixT r)
#endif
gatherUInfixT t = ILNil t

-- This can fail due to incompatible fixities
resolveInfixT1 :: InfixList -> TypeQ
resolveInfixT1 = go []
  where
    go :: [(Type,Name,Bool,Fixity)] -> InfixList -> TypeQ
    go ts (ILNil u) = return (foldl (\acc (l,o,p,_) -> mkConT p o `AppT` l `AppT` acc) u ts)
    go ts (ILCons l o p r) =
      do ofx <- fromMaybe defaultFixity <$> reifyFixityCompat o
         let push = go ((l,o,p,ofx):ts) r
         case ts of
           (l1,o1,p1,o1fx):ts' ->
             case compareFixity o1fx ofx of
               Just True  -> go ((mkConT p1 o1 `AppT` l1 `AppT` l, o, p, ofx):ts') r
               Just False -> push
               Nothing    -> fail (precedenceError o1 o1fx o ofx)
           _ -> push

    mkConT :: Bool -> Name -> Type
    mkConT promoted = if promoted then PromotedT else ConT

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

data InfixList
  = ILCons Type      -- The first argument to the type operator
           Name      -- The name of the infix type operator
           Bool      -- 'True' if this is a promoted infix data constructor,
                     -- 'False' otherwise
           InfixList -- The rest of the infix applications to resolve
  | ILNil Type

ilAppend :: InfixList -> Name -> Bool -> InfixList -> InfixList
ilAppend (ILNil l)            o p r = ILCons l o p r
ilAppend (ILCons l1 o1 p1 r1) o p r = ILCons l1 o1 p1 (ilAppend r1 o p r)


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

takeFieldNames :: [(Name,a,b)] -> [Name]
takeFieldNames xs = [a | (a,_,_) <- xs]

takeFieldStrictness :: [(a,Bang,b)] -> [FieldStrictness]
takeFieldStrictness xs = [normalizeStrictness a | (_,a,_) <- xs]

takeFieldTypes :: [(a,b,Type)] -> [Type]
takeFieldTypes xs = [a | (_,_,a) <- xs]

conHasRecord :: Name -> ConstructorInfo -> Bool
conHasRecord recName info =
  case constructorVariant info of
    NormalConstructor        -> False
    InfixConstructor         -> False
    RecordConstructor fields -> recName `elem` fields

------------------------------------------------------------------------

-- | Add universal quantifier for all free variables in the type. This is
-- useful when constructing a type signature for a declaration.
-- This code is careful to ensure that the order of the variables quantified
-- is determined by their order of appearance in the type signature. (In
-- contrast with being dependent upon the Ord instance for 'Name')
quantifyType :: Type -> Type
quantifyType t
  | null tvbs
  = t
  | ForallT tvbs' ctxt' t' <- t -- Collapse two consecutive foralls (#63)
  = ForallT (tvbs ++ tvbs') ctxt' t'
  | otherwise
  = ForallT tvbs [] t
  where
    tvbs = changeTVFlags SpecifiedSpec $ freeVariablesWellScoped [t]

-- | Take a list of 'Type's, find their free variables, and sort them
-- according to dependency order.
--
-- As an example of how this function works, consider the following type:
--
-- @
-- Proxy (a :: k)
-- @
--
-- Calling 'freeVariables' on this type would yield @[a, k]@, since that is
-- the order in which those variables appear in a left-to-right fashion. But
-- this order does not preserve the fact that @k@ is the kind of @a@. Moreover,
-- if you tried writing the type @forall a k. Proxy (a :: k)@, GHC would reject
-- this, since GHC would demand that @k@ come before @a@.
--
-- 'freeVariablesWellScoped' orders the free variables of a type in a way that
-- preserves this dependency ordering. If one were to call
-- 'freeVariablesWellScoped' on the type above, it would return
-- @[k, (a :: k)]@. (This is why 'freeVariablesWellScoped' returns a list of
-- 'TyVarBndr's instead of 'Name's, since it must make it explicit that @k@
-- is the kind of @a@.)
--
-- 'freeVariablesWellScoped' guarantees the free variables returned will be
-- ordered such that:
--
-- 1. Whenever an explicit kind signature of the form @(A :: K)@ is
--    encountered, the free variables of @K@ will always appear to the left of
--    the free variables of @A@ in the returned result.
--
-- 2. The constraint in (1) notwithstanding, free variables will appear in
--    left-to-right order of their original appearance.
--
-- On older GHCs, this takes measures to avoid returning explicitly bound
-- kind variables, which was not possible before @TypeInType@.
freeVariablesWellScoped :: [Type] -> [TyVarBndrUnit]
freeVariablesWellScoped tys =
  let fvs :: [Name]
      fvs = freeVariables tys

      varKindSigs :: Map Name Kind
      varKindSigs = foldMap go_ty tys
        where
          go_ty :: Type -> Map Name Kind
          go_ty (ForallT tvbs ctxt t) =
            foldr (\tvb -> Map.delete (tvName tvb))
                  (foldMap go_ty ctxt `mappend` go_ty t) tvbs
          go_ty (AppT t1 t2) = go_ty t1 `mappend` go_ty t2
          go_ty (SigT t k) =
            let kSigs = go_ty k
            in case t of
                 VarT n -> Map.insert n k kSigs
                 _      -> go_ty t `mappend` kSigs
#if MIN_VERSION_template_haskell(2,15,0)
          go_ty (AppKindT t k) = go_ty t `mappend` go_ty k
          go_ty (ImplicitParamT _ t) = go_ty t
#endif
#if MIN_VERSION_template_haskell(2,16,0)
          go_ty (ForallVisT tvbs t) =
            foldr (\tvb -> Map.delete (tvName tvb)) (go_ty t) tvbs
#endif
          go_ty _ = mempty

      -- | Do a topological sort on a list of tyvars,
      --   so that binders occur before occurrences
      -- E.g. given  [ a::k, k::*, b::k ]
      -- it'll return a well-scoped list [ k::*, a::k, b::k ]
      --
      -- This is a deterministic sorting operation
      -- (that is, doesn't depend on Uniques).
      --
      -- It is also meant to be stable: that is, variables should not
      -- be reordered unnecessarily.
      scopedSort :: [Name] -> [Name]
      scopedSort = go [] []

      go :: [Name]     -- already sorted, in reverse order
         -> [Set Name] -- each set contains all the variables which must be placed
                       -- before the tv corresponding to the set; they are accumulations
                       -- of the fvs in the sorted tvs' kinds

                       -- This list is in 1-to-1 correspondence with the sorted tyvars
                       -- INVARIANT:
                       --   all (\tl -> all (`isSubsetOf` head tl) (tail tl)) (tails fv_list)
                       -- That is, each set in the list is a superset of all later sets.
         -> [Name]     -- yet to be sorted
         -> [Name]
      go acc _fv_list [] = reverse acc
      go acc  fv_list (tv:tvs)
        = go acc' fv_list' tvs
        where
          (acc', fv_list') = insert tv acc fv_list

      insert :: Name       -- var to insert
             -> [Name]     -- sorted list, in reverse order
             -> [Set Name] -- list of fvs, as above
             -> ([Name], [Set Name])   -- augmented lists
      insert tv []     []         = ([tv], [kindFVSet tv])
      insert tv (a:as) (fvs:fvss)
        | tv `Set.member` fvs
        , (as', fvss') <- insert tv as fvss
        = (a:as', fvs `Set.union` fv_tv : fvss')

        | otherwise
        = (tv:a:as, fvs `Set.union` fv_tv : fvs : fvss)
        where
          fv_tv = kindFVSet tv

         -- lists not in correspondence
      insert _ _ _ = error "scopedSort"

      kindFVSet n =
        maybe Set.empty (Set.fromList . freeVariables) (Map.lookup n varKindSigs)
      ascribeWithKind n =
        maybe (plainTV n) (kindedTV n) (Map.lookup n varKindSigs)

  in map ascribeWithKind $ scopedSort fvs

-- | Substitute all of the free variables in a type with fresh ones
freshenFreeVariables :: Type -> Q Type
freshenFreeVariables t =
  do let xs = [ (n, VarT <$> newName (nameBase n)) | n <- freeVariables t]
     subst <- T.sequence (Map.fromList xs)
     return (applySubstitution subst t)


-- | Class for types that support type variable substitution.
class TypeSubstitution a where
  -- | Apply a type variable substitution.
  applySubstitution :: Map Name Type -> a -> a
  -- | Compute the free type variables
  freeVariables     :: a -> [Name]

instance TypeSubstitution a => TypeSubstitution [a] where
  freeVariables     = nub . concat . map freeVariables
  applySubstitution = fmap . applySubstitution

instance TypeSubstitution Type where
  applySubstitution subst = go
    where
      go (ForallT tvs context t) =
        let (subst', tvs') = substTyVarBndrs subst tvs in
        ForallT tvs'
                (applySubstitution subst' context)
                (applySubstitution subst' t)
      go (AppT f x)      = AppT (go f) (go x)
      go (SigT t k)      = SigT (go t) (applySubstitution subst k) -- k could be Kind
      go (VarT v)        = Map.findWithDefault (VarT v) v subst
      go (InfixT l c r)  = InfixT (go l) c (go r)
      go (UInfixT l c r) = UInfixT (go l) c (go r)
      go (ParensT t)     = ParensT (go t)
#if MIN_VERSION_template_haskell(2,15,0)
      go (AppKindT t k)  = AppKindT (go t) (go k)
      go (ImplicitParamT n t)
                         = ImplicitParamT n (go t)
#endif
#if MIN_VERSION_template_haskell(2,16,0)
      go (ForallVisT tvs t) =
        let (subst', tvs') = substTyVarBndrs subst tvs in
        ForallVisT tvs'
                   (applySubstitution subst' t)
#endif
#if MIN_VERSION_template_haskell(2,19,0)
      go (PromotedInfixT l c r)
                         = PromotedInfixT (go l) c (go r)
      go (PromotedUInfixT l c r)
                         = PromotedUInfixT (go l) c (go r)
#endif
      go t               = t

      subst_tvbs :: [TyVarBndr_ flag] -> (Map Name Type -> a) -> a
      subst_tvbs tvs k = k $ foldl' (flip Map.delete) subst (map tvName tvs)

  freeVariables t =
    case t of
      ForallT tvs context t' ->
          fvs_under_forall tvs (freeVariables context `union` freeVariables t')
      AppT f x      -> freeVariables f `union` freeVariables x
      SigT t' k     -> freeVariables t' `union` freeVariables k
      VarT v        -> [v]
      InfixT l _ r  -> freeVariables l `union` freeVariables r
      UInfixT l _ r -> freeVariables l `union` freeVariables r
      ParensT t'    -> freeVariables t'
#if MIN_VERSION_template_haskell(2,15,0)
      AppKindT t k  -> freeVariables t `union` freeVariables k
      ImplicitParamT _ t
                    -> freeVariables t
#endif
#if MIN_VERSION_template_haskell(2,16,0)
      ForallVisT tvs t'
                    -> fvs_under_forall tvs (freeVariables t')
#endif
#if MIN_VERSION_template_haskell(2,19,0)
      PromotedInfixT l _ r
                    -> freeVariables l `union` freeVariables r
      PromotedUInfixT l _ r
                    -> freeVariables l `union` freeVariables r
#endif
      _             -> []
    where
      fvs_under_forall :: [TyVarBndr_ flag] -> [Name] -> [Name]
      fvs_under_forall tvs fvs =
        (freeVariables (map tvKind tvs) `union` fvs)
        \\ map tvName tvs

instance TypeSubstitution ConstructorInfo where
  freeVariables ci =
      (freeVariables (map tvKind (constructorVars ci))
          `union` freeVariables (constructorContext ci)
          `union` freeVariables (constructorFields ci))
      \\ (tvName <$> constructorVars ci)

  applySubstitution subst ci =
    let subst' = foldl' (flip Map.delete) subst (map tvName (constructorVars ci)) in
    ci { constructorVars    = map (mapTVKind (applySubstitution subst'))
                                  (constructorVars ci)
       , constructorContext = applySubstitution subst' (constructorContext ci)
       , constructorFields  = applySubstitution subst' (constructorFields ci)
       }

-- | Substitutes into the kinds of type variable binders. This makes an effort
-- to avoid capturing the 'TyVarBndr' names during substitution by
-- alpha-renaming names if absolutely necessary. For a version of this function
-- which does /not/ avoid capture, see 'substTyVarBndrKinds'.
substTyVarBndrs :: Map Name Type -> [TyVarBndr_ flag] -> (Map Name Type, [TyVarBndr_ flag])
substTyVarBndrs = mapAccumL substTyVarBndr

-- | The workhorse for 'substTyVarBndrs'.
substTyVarBndr :: Map Name Type -> TyVarBndr_ flag -> (Map Name Type, TyVarBndr_ flag)
substTyVarBndr subst tvb
  | tvbName `Map.member` subst
  = (Map.delete tvbName subst, mapTVKind (applySubstitution subst) tvb)
  | tvbName `Set.notMember` substRangeFVs
  = (subst, mapTVKind (applySubstitution subst) tvb)
  | otherwise
  = let tvbName' = evade tvbName in
    ( Map.insert tvbName (VarT tvbName') subst
    , mapTV (\_ -> tvbName') id (applySubstitution subst) tvb
    )
  where
    tvbName :: Name
    tvbName = tvName tvb

    substRangeFVs :: Set Name
    substRangeFVs = Set.fromList $ freeVariables $ Map.elems subst

    evade :: Name -> Name
    evade n | n `Set.member` substRangeFVs
            = evade $ bump n
            | otherwise
            = n

    -- An improvement would be to try a variety of different characters instead
    -- of prepending the same character repeatedly. Let's wait to see if
    -- someone complains about this before making this more complicated,
    -- however.
    bump :: Name -> Name
    bump n = mkName $ 'f':nameBase n

-- | Substitutes into the kinds of type variable binders. This is slightly more
-- efficient than 'substTyVarBndrs', but at the expense of not avoiding
-- capture. Only use this function in situations where you know that none of
-- the 'TyVarBndr' names are contained in the range of the substitution.
substTyVarBndrKinds :: Map Name Type -> [TyVarBndr_ flag] -> [TyVarBndr_ flag]
substTyVarBndrKinds subst = map (substTyVarBndrKind subst)

-- | The workhorse for 'substTyVarBndrKinds'.
substTyVarBndrKind :: Map Name Type -> TyVarBndr_ flag -> TyVarBndr_ flag
substTyVarBndrKind subst = mapTVKind (applySubstitution subst)

------------------------------------------------------------------------

combineSubstitutions :: Map Name Type -> Map Name Type -> Map Name Type
combineSubstitutions x y = Map.union (fmap (applySubstitution y) x) y

-- | Compute the type variable substitution that unifies a list of types,
-- or fail in 'Q'.
--
-- All infix issue should be resolved before using 'unifyTypes'
--
-- Alpha equivalent quantified types are not unified.
unifyTypes :: [Type] -> Q (Map Name Type)
unifyTypes [] = return Map.empty
unifyTypes (t:ts) =
  do t':ts' <- mapM resolveTypeSynonyms (t:ts)
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
                  | otherwise                = Right (Map.singleton n t)
unify' t (VarT n) | n `elem` freeVariables t = Left (VarT n, t)
                  | otherwise                = Right (Map.singleton n t)

unify' (AppT f1 x1) (AppT f2 x2) =
  do sub1 <- unify' f1 f2
     sub2 <- unify' (applySubstitution sub1 x1) (applySubstitution sub1 x2)
     Right (combineSubstitutions sub1 sub2)

-- Doesn't unify kind signatures
unify' (SigT t _) u = unify' t u
unify' t (SigT u _) = unify' t u

-- only non-recursive cases should remain at this point
unify' t u
  | t == u    = Right Map.empty
  | otherwise = Left (t,u)


-- | Construct an equality constraint. The implementation of 'Pred' varies
-- across versions of Template Haskell.
equalPred :: Type -> Type -> Pred
equalPred x y = AppT (AppT EqualityT x) y

-- | Construct a typeclass constraint. The implementation of 'Pred' varies
-- across versions of Template Haskell.
classPred :: Name {- ^ class -} -> [Type] {- ^ parameters -} -> Pred
classPred = foldl AppT . ConT

-- | Match a 'Pred' representing an equality constraint. Returns
-- arguments to the equality constraint if successful.
asEqualPred :: Pred -> Maybe (Type,Type)
asEqualPred (EqualityT `AppT` x `AppT` y)                    = Just (x,y)
asEqualPred (ConT eq   `AppT` x `AppT` y) | eq == eqTypeName = Just (x,y)
asEqualPred _                                                = Nothing

-- | Match a 'Pred' representing a class constraint.
-- Returns the classname and parameters if successful.
asClassPred :: Pred -> Maybe (Name, [Type])
asClassPred t =
  case decomposeType t of
    ConT f :| xs | f /= eqTypeName -> Just (f,xs)
    _                              -> Nothing

------------------------------------------------------------------------

-- | If we are working with a 'Dec' obtained via 'reify' (as opposed to one
-- created from, say, [d| ... |] quotes), then we need to apply more hacks than
-- we otherwise would to sanitize the 'Dec'. See #28.
type IsReifiedDec = Bool

isReified, isn'tReified :: IsReifiedDec
isReified    = True
isn'tReified = False

-- On old versions of GHC, reify would not give you kind signatures for
-- GADT type variables of kind *. To work around this, we insert the kinds
-- manually on any reified type variable binders without a signature. However,
-- don't do this for quoted type variable binders (#84).

giveDIVarsStarKinds :: IsReifiedDec -> DatatypeInfo -> DatatypeInfo
giveDIVarsStarKinds isReified info =
  info { datatypeVars      = map (giveTyVarBndrStarKind isReified) (datatypeVars info)
       , datatypeInstTypes = map (giveTypeStarKind isReified) (datatypeInstTypes info) }

giveCIVarsStarKinds :: IsReifiedDec -> ConstructorInfo -> ConstructorInfo
giveCIVarsStarKinds isReified info =
  info { constructorVars = map (giveTyVarBndrStarKind isReified) (constructorVars info) }

giveTyVarBndrStarKind :: IsReifiedDec ->  TyVarBndrUnit -> TyVarBndrUnit
giveTyVarBndrStarKind isReified tvb
  | isReified
  = elimTV (\n -> kindedTV n starK) (\_ _ -> tvb) tvb
  | otherwise
  = tvb

giveTypeStarKind :: IsReifiedDec -> Type -> Type
giveTypeStarKind isReified t
  | isReified
  = case t of
      VarT n -> SigT t starK
      _      -> t
  | otherwise
  = t

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
  do s <- T.sequence (Map.fromList substList)
     return info { datatypeCons = applySubstitution s (datatypeCons info) }

  where
    used  = freeVariables (datatypeCons info)
    bound = map tvName (datatypeVars info)
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
  CxtQ           {- ^ context                 -} ->
  Name           {- ^ type constructor        -} ->
  [TyVarBndrVis] {- ^ type parameters         -} ->
  [ConQ]         {- ^ constructor definitions -} ->
  [Name]         {- ^ derived class names     -} ->
  DecQ
#if MIN_VERSION_template_haskell(2,12,0)
dataDCompat c n ts cs ds =
  dataD c n ts Nothing cs
    (if null ds then [] else [derivClause Nothing (map conT ds)])
#else
dataDCompat c n ts cs ds =
  dataD c n ts Nothing cs
    (return (map ConT ds))
#endif

-- | Backward compatible version of 'newtypeD'
newtypeDCompat ::
  CxtQ           {- ^ context                 -} ->
  Name           {- ^ type constructor        -} ->
  [TyVarBndrVis] {- ^ type parameters         -} ->
  ConQ           {- ^ constructor definition  -} ->
  [Name]         {- ^ derived class names     -} ->
  DecQ
#if MIN_VERSION_template_haskell(2,12,0)
newtypeDCompat c n ts cs ds =
  newtypeD c n ts Nothing cs
    (if null ds then [] else [derivClause Nothing (map conT ds)])
#else
newtypeDCompat c n ts cs ds =
  newtypeD c n ts Nothing cs
    (return (map ConT ds))
#endif

-- | Backward compatible version of 'tySynInstD'
tySynInstDCompat ::
  Name                    {- ^ type family name    -}   ->
  Maybe [Q TyVarBndrUnit] {- ^ type variable binders -} ->
  [TypeQ]                 {- ^ instance parameters -}   ->
  TypeQ                   {- ^ instance result     -}   ->
  DecQ
#if MIN_VERSION_template_haskell(2,15,0)
tySynInstDCompat n mtvbs ps r = TySynInstD <$> (TySynEqn <$> mapM sequence mtvbs
                                                         <*> foldl' appT (conT n) ps
                                                         <*> r)
#else
tySynInstDCompat n _ ps r     = TySynInstD n <$> (TySynEqn <$> sequence ps <*> r)
#endif

-- | Backward compatible version of 'pragLineD'. Returns
-- 'Nothing' if line pragmas are not suported.
pragLineDCompat ::
  Int     {- ^ line number -} ->
  String  {- ^ file name   -} ->
  Maybe DecQ
pragLineDCompat ln fn = Just (pragLineD ln fn)

arrowKCompat :: Kind -> Kind -> Kind
arrowKCompat x y = arrowK `appK` x `appK` y

------------------------------------------------------------------------

-- | Backwards compatibility wrapper for 'Fixity' lookup.
--
-- In @template-haskell-2.11.0.0@ and later, the answer will always
-- be 'Just' of a fixity.
--
-- Before @template-haskell-2.11.0.0@ it was only possible to determine
-- fixity information for variables, class methods, and data constructors.
-- In this case for type operators the answer could be 'Nothing', which
-- indicates that the answer is unavailable.
reifyFixityCompat :: Name -> Q (Maybe Fixity)
reifyFixityCompat n = recover (return Nothing) ((`mplus` Just defaultFixity) <$> reifyFixity n)

-- | Call 'reify' and return @'Just' info@ if successful or 'Nothing' if
-- reification failed.
reifyMaybe :: Name -> Q (Maybe Info)
reifyMaybe n = return Nothing `recover` fmap Just (reify n)
