{-# Language CPP, DeriveDataTypeable #-}

#if MIN_VERSION_base(4,4,0)
#define HAS_GENERICS
{-# Language DeriveGeneric #-}
#endif

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
 , 'datatypeVars'    = [ 'SigT' ('VarT' a_3530822107858468866) 'StarT' ]
 , 'datatypeVariant' = 'Datatype'
 , 'datatypeCons'    =
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
  , asEqualPred
  , asClassPred

  -- * Backward compatible data definitions
  , dataDCompat
  , arrowKCompat

  -- * Strictness annotations
  , isStrictAnnot
  , notStrictAnnot
  , unpackedAnnot

  -- * Type simplification
  , resolveTypeSynonyms
  , resolveInfixT

  -- * Fixities
  , reifyFixityCompat
  , showFixity
  , showFixityDirection

  -- * Convenience functions
  , unifyTypes
  , tvName
  , datatypeType
  ) where

import           Data.Data (Typeable, Data)
import           Data.Foldable (foldMap, foldl')
import           Data.List (find, union, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Traversable as T
import           Control.Monad
import           Language.Haskell.TH
#if MIN_VERSION_template_haskell(2,11,0)
                                     hiding (Extension(..))
#endif
import           Language.Haskell.TH.Datatype.Internal
import           Language.Haskell.TH.Lib (arrowK, starK) -- needed for th-2.4

#ifdef HAS_GENERICS
import           GHC.Generics (Generic)
#endif

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative(..), (<$>))
#endif

-- | Normalized information about newtypes and data types.
--
-- 'datatypeVars' types will have an outermost 'SigT' to indicate the
-- parameter's kind. These types will be simple variables for /ADT/s
-- declared with @data@ and @newtype@, but can be more complex for
-- types declared with @data instance@ and @newtype instance@.
data DatatypeInfo = DatatypeInfo
  { datatypeContext :: Cxt               -- ^ Data type context (deprecated)
  , datatypeName    :: Name              -- ^ Type constructor
  , datatypeVars    :: [Type]            -- ^ Type parameters
  , datatypeVariant :: DatatypeVariant   -- ^ Extra information
  , datatypeCons    :: [ConstructorInfo] -- ^ Normalize constructor information
  }
  deriving (Show, Eq, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
           )

-- | Possible variants of data type declarations.
data DatatypeVariant
  = Datatype        -- ^ Type declared with @data@
  | Newtype         -- ^ Type declared with @newtype@
  | DataInstance    -- ^ Type declared with @data instance@
  | NewtypeInstance -- ^ Type declared with @newtype instance@
  deriving (Show, Read, Eq, Ord, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
           )

-- | Normalized information about constructors associated with newtypes and
-- data types.
data ConstructorInfo = ConstructorInfo
  { constructorName       :: Name               -- ^ Constructor name
  , constructorVars       :: [TyVarBndr]        -- ^ Constructor type parameters
  , constructorContext    :: Cxt                -- ^ Constructor constraints
  , constructorFields     :: [Type]             -- ^ Constructor fields
  , constructorStrictness :: [FieldStrictness]  -- ^ Constructor fields' strictness
                                                --   (Invariant: has the same length
                                                --   as constructorFields)
  , constructorVariant    :: ConstructorVariant -- ^ Extra information
  }
  deriving (Show, Eq, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
           )

-- | Possible variants of data constructors.
data ConstructorVariant
  = NormalConstructor        -- ^ Constructor without field names
  | InfixConstructor         -- ^ Constructor without field names that is
                             --   declared infix
  | RecordConstructor [Name] -- ^ Constructor with field names
  deriving (Show, Eq, Ord, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
           )

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
  deriving (Show, Eq, Ord, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
           )

-- | Information about a constructor field's unpackedness annotation.
data Unpackedness
  = UnspecifiedUnpackedness -- ^ No annotation whatsoever
  | NoUnpack                -- ^ Annotated with @{\-\# NOUNPACK \#-\}@
  | Unpack                  -- ^ Annotated with @{\-\# UNPACK \#-\}@
  deriving (Show, Eq, Ord, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
           )

-- | Information about a constructor field's strictness annotation.
data Strictness
  = UnspecifiedStrictness -- ^ No annotation whatsoever
  | Lazy                  -- ^ Annotated with @~@
  | Strict                -- ^ Annotated with @!@
  deriving (Show, Eq, Ord, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
           )

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
  $ datatypeVars di


-- | Compute a normalized view of the metadata about a data type or newtype
-- given a constructor.
--
-- This function will accept any constructor (value or type) for a type
-- declared with newtype or data. Value constructors must be used to
-- lookup datatype information about /data instances/ and /newtype instances/.
--
-- GADT constructors are normalized into datatypes with explicit equality
-- constraints.
--
-- This function will apply various bug-fixes to the output of the underlying
-- @template-haskell@ library in order to provide a view of datatypes in
-- as uniform a way as possible.
reifyDatatype ::
  Name {- ^ constructor -} ->
  Q DatatypeInfo
reifyDatatype n = normalizeInfo' "reifyDatatype" =<< reify n


-- | Normalize 'Info' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
normalizeInfo :: Info -> Q DatatypeInfo
normalizeInfo = normalizeInfo' "normalizeInfo"

normalizeInfo' :: String -> Info -> Q DatatypeInfo
normalizeInfo' entry i =
  case i of
    PrimTyConI{}                      -> bad "Primitive type not supported"
    ClassI{}                          -> bad "Class not supported"
#if MIN_VERSION_template_haskell(2,11,0)
    FamilyI DataFamilyD{} _           ->
#elif MIN_VERSION_template_haskell(2,7,0)
    FamilyI (FamilyD DataFam _ _ _) _ ->
#else
    TyConI (FamilyD DataFam _ _ _)    ->
#endif
                                         bad "Use a value constructor to reify a data family instance"
#if MIN_VERSION_template_haskell(2,7,0)
    FamilyI _ _                       -> bad "Type families not supported"
#endif
    TyConI dec                        -> normalizeDec dec
#if MIN_VERSION_template_haskell(2,11,0)
    DataConI name _ parent            -> reifyParent name parent
#elif MIN_VERSION_template_haskell(2,7,0)
    DataConI name _ parent _          -> reifyParent name parent
#else
    -- Give a sensible error message if you try to look up a data family
    -- instance constructor in GHC 7.0 or 7.2
    DataConI{}                        -> bad $ "Data family instances can only " ++
                                               "be reified with GHC 7.4 or later"
#endif
    _                                 -> bad "Expected a type constructor"
  where
    bad msg = fail (entry ++ ": " ++ msg)


reifyParent :: Name -> Name -> Q DatatypeInfo
reifyParent con parent =
  do info <- reify parent
     case info of
       TyConI dec -> normalizeDec dec
#if MIN_VERSION_template_haskell(2,7,0)
       FamilyI dec instances ->
         do let instances1 = map (repairDataFam dec) instances
            instances2 <- mapM normalizeDec instances1
            case find p instances2 of
              Just inst -> return inst
              Nothing   -> fail "PANIC: reifyParent lost the instance"
#endif
       _ -> fail "PANIC: reifyParent unexpected parent"
  where
    p info = con `elem` map constructorName (datatypeCons info)

#if MIN_VERSION_template_haskell(2,8,0) && (!MIN_VERSION_template_haskell(2,10,0))
    kindPart (KindedTV _ k) = [k]
    kindPart (PlainTV  _  ) = []

    -- A version of repairVarKindsWith that does much more extra work to
    -- (1) eta-expand missing type patterns, and (2) ensure that the kind
    -- signatures for these new type patterns match accordingly.
    repairVarKindsWith' :: [TyVarBndr] -> [Type] -> [Type]
    repairVarKindsWith' dvars ts =
      let nparams             = length dvars
          kparams             = kindVars dvars
          (tsKinds,tsNoKinds) = splitAt (length kparams) ts
          tsKinds'            = map sanitizeStars tsKinds
          extraTys            = drop (length tsNoKinds) (bndrParams dvars)
          ts'                 = tsNoKinds ++ extraTys -- eta-expand
      in applySubstitution (Map.fromList (zip kparams tsKinds')) $
         repairVarKindsWith dvars ts'

    -- A GHC 7.6-specific bug requires us to replace all occurrences of
    -- (ConT GHC.Prim.*) with StarT, or else Template Haskell will reject it.
    -- Luckily, (ConT GHC.Prim.*) only seems to occur in this one spot.
    sanitizeStars :: Kind -> Kind
    sanitizeStars = go
      where
        go :: Kind -> Kind
        go (AppT t1 t2)                 = AppT (go t1) (go t2)
        go (SigT t k)                   = SigT (go t) (go k)
        go (ConT n) | n == starKindName = StarT
        go t                            = t

    kindVars = freeVariables . map kindPart

    -- Sadly, Template Haskell's treatment of data family instances leaves much
    -- to be desired. Here are some problems that we have to work around:
    --
    -- 1. On all versions of GHC, TH leaves off the kind signatures on the
    --    type patterns of data family instances where a kind signature isn't
    --    specified explicitly. Here, we can use the parent data family's
    --    type variable binders to reconstruct the kind signatures if they
    --    are missing.
    -- 2. On GHC 7.6 and 7.8, TH will eta-reduce data instances. We can find
    --    the missing type variables on the data constructor.
    --
    -- We opt to avoid propagating these new type variables through to the
    -- constructor now, but we will return to this task in normalizeCon.
    repairDataFam
      (FamilyD _ _ dvars _)
      (NewtypeInstD cx n ts con deriv) =
        NewtypeInstD cx n (repairVarKindsWith' dvars ts) con deriv
    repairDataFam
      (FamilyD _ _ dvars _)
      (DataInstD cx n ts cons deriv) =
        DataInstD cx n (repairVarKindsWith' dvars ts) cons deriv
#else
    repairDataFam famD instD
# if MIN_VERSION_template_haskell(2,11,0)
      | DataFamilyD _ dvars _ <- famD
      , NewtypeInstD cx n ts k c deriv <- instD
      = NewtypeInstD cx n (repairVarKindsWith dvars ts) k c deriv

      | DataFamilyD _ dvars _ <- famD
      , DataInstD cx n ts k c deriv <- instD
      = DataInstD cx n (repairVarKindsWith dvars ts) k c deriv
# else
      | FamilyD _ _ dvars _ <- famD
      , NewtypeInstD cx n ts c deriv <- instD
      = NewtypeInstD cx n (repairVarKindsWith dvars ts) c deriv

      | FamilyD _ _ dvars _ <- famD
      , DataInstD cx n ts c deriv <- instD
      = DataInstD cx n (repairVarKindsWith dvars ts) c deriv
# endif
#endif
    repairDataFam _ instD = instD

repairVarKindsWith :: [TyVarBndr] -> [Type] -> [Type]
repairVarKindsWith = zipWith stealKindForType

-- If a VarT is missing an explicit kind signature, steal it from a TyVarBndr.
stealKindForType :: TyVarBndr -> Type -> Type
stealKindForType tvb t@VarT{} = SigT t (tvbKind tvb)
stealKindForType _   t        = t

-- | Normalize 'Dec' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
--
-- Beware: 'normalizeDec' can have surprising behavior when it comes to fixity.
-- For instance, if you have this quasiquoted data declaration:
--
-- [d| infix 5 :^^:
--     data Foo where
--       (:^^:) :: Int -> Int -> Foo |]
--
-- Then if you pass the 'Dec' for @Foo@ to 'normalizeDec' without splicing it
-- in a previous Template Haskell splice, then @(:^^:) will be labeled a 'NormalConstructor'
-- instead of an 'InfixConstructor'. This is because Template Haskell has no way to
-- reify the fixity declaration for @(:^^:)@, so it must assume there isn't one. To
-- work around this behavior, use 'reifyDatatype' instead.
normalizeDec :: Dec -> Q DatatypeInfo
#if MIN_VERSION_template_haskell(2,12,0)
normalizeDec (NewtypeD context name tyvars _kind con _derives) =
  giveTypesStarKinds <$> normalizeDec' context name (bndrParams tyvars) [con] Newtype
normalizeDec (DataD context name tyvars _kind cons _derives) =
  giveTypesStarKinds <$> normalizeDec' context name (bndrParams tyvars) cons Datatype
normalizeDec (NewtypeInstD context name params _kind con _derives) =
  repair13618 . giveTypesStarKinds =<<
  normalizeDec' context name params [con] NewtypeInstance
normalizeDec (DataInstD context name params _kind cons _derives) =
  repair13618 . giveTypesStarKinds =<<
  normalizeDec' context name params cons DataInstance
#elif MIN_VERSION_template_haskell(2,11,0)
normalizeDec (NewtypeD context name tyvars _kind con _derives) =
  giveTypesStarKinds <$> normalizeDec' context name (bndrParams tyvars) [con] Newtype
normalizeDec (DataD context name tyvars _kind cons _derives) =
  giveTypesStarKinds <$> normalizeDec' context name (bndrParams tyvars) cons Datatype
normalizeDec (NewtypeInstD context name params _kind con _derives) =
  repair13618 . giveTypesStarKinds =<<
  normalizeDec' context name params [con] NewtypeInstance
normalizeDec (DataInstD context name params _kind cons _derives) =
  repair13618 . giveTypesStarKinds =<<
  normalizeDec' context name params cons DataInstance
#else
normalizeDec (NewtypeD context name tyvars con _derives) =
  giveTypesStarKinds <$> normalizeDec' context name (bndrParams tyvars) [con] Newtype
normalizeDec (DataD context name tyvars cons _derives) =
  giveTypesStarKinds <$> normalizeDec' context name (bndrParams tyvars) cons Datatype
normalizeDec (NewtypeInstD context name params con _derives) =
  repair13618 . giveTypesStarKinds =<<
  normalizeDec' context name params [con] NewtypeInstance
normalizeDec (DataInstD context name params cons _derives) =
  repair13618 . giveTypesStarKinds =<<
  normalizeDec' context name params cons DataInstance
#endif
normalizeDec _ = fail "reifyDatatype: DataD or NewtypeD required"

bndrParams :: [TyVarBndr] -> [Type]
bndrParams = map $ \bndr ->
  case bndr of
    KindedTV t k -> SigT (VarT t) k
    PlainTV  t   -> VarT t

-- | Extract the kind from a 'TyVarBndr'. Assumes 'PlainTV' has kind @*@.
tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _)   = starK
tvbKind (KindedTV _ k) = k

-- | Remove the outermost 'SigT'.
stripSigT :: Type -> Type
stripSigT (SigT t _) = t
stripSigT t          = t


normalizeDec' ::
  Cxt             {- ^ Datatype context    -} ->
  Name            {- ^ Type constructor    -} ->
  [Type]          {- ^ Type parameters     -} ->
  [Con]           {- ^ Constructors        -} ->
  DatatypeVariant {- ^ Extra information   -} ->
  Q DatatypeInfo
normalizeDec' context name params cons variant =
  do cons' <- concat <$> mapM (normalizeCon name params variant) cons
     return DatatypeInfo
       { datatypeContext = context
       , datatypeName    = name
       , datatypeVars    = params
       , datatypeCons    = cons'
       , datatypeVariant = variant
       }

-- | Normalize a 'Con' into a 'ConstructorInfo'. This requires knowledge of
-- the type and parameters of the constructor, as well as whether the constructor
-- is for a data family instance, as extracted from the outer
-- 'Dec'.
normalizeCon ::
  Name            {- ^ Type constructor  -} ->
  [Type]          {- ^ Type parameters   -} ->
  DatatypeVariant {- ^ Extra information -} ->
  Con             {- ^ Constructor       -} ->
  Q [ConstructorInfo]
normalizeCon typename params variant = fmap (map giveTyVarBndrsStarKinds) . dispatch
  where
    -- A GADT constructor is declared infix when:
    --
    -- 1. Its name uses operator syntax (e.g., (:*:))
    -- 2. It has exactly two fields
    -- 3. It has a programmer-supplied fixity declaration
    checkGadtFixity :: [Type] -> Name -> Q ConstructorVariant
    checkGadtFixity ts n = do
#if MIN_VERSION_template_haskell(2,11,0)
      -- Don't call reifyFixityCompat here! We need to be able to distinguish
      -- between a default fixity and an explicit @infixl 9@.
      mbFi <- return Nothing `recover` reifyFixity n
      let userSuppliedFixity = isJust mbFi
#else
      -- On old GHCs, there is a bug where infix GADT constructors will
      -- mistakenly be marked as (ForallC (NormalC ...)) instead of
      -- (ForallC (InfixC ...)). This is especially annoying since on these
      -- versions of GHC, Template Haskell doesn't grant the ability to query
      -- whether a constructor was given a user-supplied fixity declaration.
      -- Rather, you can only check the fixity that GHC ultimately decides on
      -- for a constructor, regardless of whether it was a default fixity or
      -- it was user-supplied.
      --
      -- We can approximate whether a fixity was user-supplied by checking if
      -- it is not equal to defaultFixity (infixl 9). Unfortunately,
      -- there is no way to distinguish between a user-supplied fixity of
      -- infixl 9 and the fixity that GHC defaults to, so we cannot properly
      -- handle that case.
      mbFi <- reifyFixityCompat n
      let userSuppliedFixity = isJust mbFi && mbFi /= Just defaultFixity
#endif
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
              go :: [TyVarBndr]
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
                    go (tyvars'++tyvars) (context'++context) True c'
#if MIN_VERSION_template_haskell(2,11,0)
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
                  gadtCase = normalizeGadtC typename params tyvars context
#endif
#if MIN_VERSION_template_haskell(2,8,0) && (!MIN_VERSION_template_haskell(2,10,0))
          dataFamCompatCase :: Con -> Q [ConstructorInfo]
          dataFamCompatCase = go []
            where
              go tyvars c =
                case c of
                  NormalC n xs ->
                    let stricts = map (normalizeStrictness . fst) xs in
                    dataFamCase' n tyvars stricts NormalConstructor
                  InfixC l n r ->
                    let stricts = map (normalizeStrictness . fst) [l,r] in
                    dataFamCase' n tyvars stricts InfixConstructor
                  RecC n xs ->
                    let stricts = takeFieldStrictness xs in
                    dataFamCase' n tyvars stricts
                                 (RecordConstructor (takeFieldNames xs))
                  ForallC tyvars' context' c' ->
                    go (tyvars'++tyvars) c'

          dataFamCase' :: Name -> [TyVarBndr] -> [FieldStrictness]
                       -> ConstructorVariant
                       -> Q [ConstructorInfo]
          dataFamCase' n tyvars stricts variant = do
            info <- reifyRecover n $ fail $ unlines
                      [ "normalizeCon: Cannot reify constructor " ++ nameBase n
                      , "You are likely calling normalizeDec on GHC 7.6 or 7.8 on a data family"
                      , "whose type variables have been eta-reduced due to GHC Trac #9692."
                      , "Unfortunately, without being able to reify the constructor's type,"
                      , "there is no way to recover the eta-reduced type variables in general."
                      , "A recommended workaround is to use reifyDatatype instead."
                      ]
            case info of
              DataConI _ ty _ _ -> do
                let (context, argTys :|- returnTy) = uncurryType ty
                returnTy' <- resolveTypeSynonyms returnTy
                -- Notice that we've ignored the Cxt and argument Types from the
                -- Con argument above, as they might be scoped over eta-reduced
                -- variables. Instead of trying to figure out what the
                -- eta-reduced variables should be substituted with post facto,
                -- we opt for the simpler approach of using the context and
                -- argument types from the reified constructor Info, which will
                -- at least be correctly scoped. This will make the task of
                -- substituting those types with the variables we put in
                -- place of the eta-reduced variables (in normalizeDec)
                -- much easier.
                normalizeGadtC typename params tyvars context [n]
                               returnTy' argTys stricts (const $ return variant)
              _ -> fail "normalizeCon: impossible"

          -- A very ad hoc way of determining if we need to perform some extra passes
          -- to repair an eta-reduction bug for data family instances that only occurs
          -- with GHC 7.6 and 7.8. We want to avoid doing these passes if at all possible,
          -- since they require reifying extra information, and reifying during
          -- normalization can be problematic for locally declared Template Haskell
          -- splices (see ##22).
          mightHaveBeenEtaReduced :: [Type] -> Bool
          mightHaveBeenEtaReduced ts =
            case unsnoc ts of
              Nothing -> False
              Just (initTs,lastT) ->
                case varTName lastT of
                  Nothing -> False
                  Just n  -> not (n `elem` freeVariables initTs)

          -- If the list is empty returns 'Nothing', otherwise returns the 'init' and the 'last'.
          unsnoc :: [a] -> Maybe ([a], a)
          unsnoc [] = Nothing
          unsnoc [x] = Just ([], x)
          unsnoc (x:xs) = Just (x:a, b)
              where Just (a,b) = unsnoc xs

          -- If a Type is a VarT, find Just its Name. Otherwise, return Nothing.
          varTName :: Type -> Maybe Name
          varTName (SigT t _) = varTName t
          varTName (VarT n)   = Just n
          varTName _          = Nothing

      in case variant of
           -- On GHC 7.6 and 7.8, there's quite a bit of post-processing that
           -- needs to be performed to work around an old bug that eta-reduces the
           -- type patterns of data families.
           DataInstance
             | mightHaveBeenEtaReduced params
             -> dataFamCompatCase
           NewtypeInstance
             | mightHaveBeenEtaReduced params
             -> dataFamCompatCase
           _ -> defaultCase
#else
      in defaultCase
#endif

#if MIN_VERSION_template_haskell(2,11,0)
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
#else
normalizeStrictness :: Strict -> FieldStrictness
normalizeStrictness IsStrict  = isStrictAnnot
normalizeStrictness NotStrict = notStrictAnnot
# if MIN_VERSION_template_haskell(2,7,0)
normalizeStrictness Unpacked  = unpackedAnnot
# endif
#endif

normalizeGadtC ::
  Name              {- ^ Type constructor             -} ->
  [Type]            {- ^ Type parameters              -} ->
  [TyVarBndr]       {- ^ Constructor parameters       -} ->
  Cxt               {- ^ Constructor context          -} ->
  [Name]            {- ^ Constructor names            -} ->
  Type              {- ^ Declared type of constructor -} ->
  [Type]            {- ^ Constructor field types      -} ->
  [FieldStrictness] {- ^ Constructor field strictness -} ->
  (Name -> Q ConstructorVariant)
                    {- ^ Determine a constructor variant
                         from its 'Name' -}              ->
  Q [ConstructorInfo]
normalizeGadtC typename params tyvars context names innerType
               fields stricts getVariant =
  do innerType' <- resolveTypeSynonyms innerType
     case decomposeType innerType' of
       ConT innerTyCon :| ts | typename == innerTyCon ->

         let (substName, context1) = mergeArguments params ts
             subst   = VarT <$> substName
             tyvars' = [ tv | tv <- tyvars, Map.notMember (tvName tv) subst ]

             context2 = applySubstitution subst (context1 ++ context)
             fields'  = applySubstitution subst fields
         in sequence [ ConstructorInfo name tyvars' context2
                                       fields' stricts <$> variantQ
                     | name <- names
                     , let variantQ = getVariant name
                     ]

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
        _ -> (subst, equalPred (VarT n) p : context)

    aux _ sc = sc

    -- Be careful not to use equalPred on GHC 8 or later! Why? Because you can
    -- have GADTs that introduce kind equalities, e.g.,
    --
    --  data T :: k -> * where T :: T Int
    --
    -- If you use equalPred here to equate the type parameter with Int, you end
    -- up with (a :: k) ~ (Int :: *), which is wrong, as the two types are of
    -- different kinds. The solution is to use heterogenous equality instead
    -- so that you have (a :: k) ~~ (Int :: *), which is kind-correct.
    hequalPred :: Type -> Type -> Pred
#if MIN_VERSION_base(4,9,0)
    hequalPred x y = AppT (AppT (ConT heqTypeName) x) y
#else
    hequalPred = equalPred
#endif

-- | Expand all of the type synonyms in a type.
resolveTypeSynonyms :: Type -> Q Type
resolveTypeSynonyms t =
  let f :| xs = decomposeType t

      notTypeSynCase = foldl AppT f <$> mapM resolveTypeSynonyms xs in

  case f of
    ConT n ->
      do info <- reifyRecover n $ fail
                   "resolveTypeSynonyms: Cannot reify type synonym information"
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

#if MIN_VERSION_template_haskell(2,8,0) && (!MIN_VERSION_template_haskell(2,10,0))
data NonEmptySnoc a = [a] :|- a

-- Decompose a function type into its context, argument types,
-- and return types. For instance, this
--
--   (Show a, b ~ Int) => (a -> b) -> Char -> Int
--
-- becomes
--
--   ([Show a, b ~ Int], [a -> b, Char] :|- Int)
uncurryType :: Type -> (Cxt, NonEmptySnoc Type)
uncurryType = go [] []
  where
    go ctxt args (AppT (AppT ArrowT t1) t2) = go ctxt (t1:args) t2
    go ctxt args (ForallT _ ctxt' t)        = go (ctxt++ctxt') args t
    go ctxt args t                          = (ctxt, reverse args :|- t)
#endif

-- | Resolve any infix type application in a type using the fixities that
-- are currently available. Starting in `template-haskell-2.11` types could
-- contain unresolved infix applications.
resolveInfixT :: Type -> Q Type

#if MIN_VERSION_template_haskell(2,11,0)
resolveInfixT (ForallT vs cx t) = forallT vs (mapM resolveInfixT cx) (resolveInfixT t)
resolveInfixT (f `AppT` x)      = resolveInfixT f `appT` resolveInfixT x
resolveInfixT (ParensT t)       = resolveInfixT t
resolveInfixT (InfixT l o r)    = conT o `appT` resolveInfixT l `appT` resolveInfixT r
resolveInfixT (SigT t k)        = SigT <$> resolveInfixT t <*> resolveInfixT k
resolveInfixT t@UInfixT{}       = resolveInfixT =<< resolveInfixT1 (gatherUInfixT t)
resolveInfixT t                 = return t

gatherUInfixT :: Type -> InfixList
gatherUInfixT (UInfixT l o r) = ilAppend (gatherUInfixT l) o (gatherUInfixT r)
gatherUInfixT t = ILNil t

-- This can fail due to incompatible fixities
resolveInfixT1 :: InfixList -> TypeQ
resolveInfixT1 = go []
  where
    go :: [(Type,Name,Fixity)] -> InfixList -> TypeQ
    go ts (ILNil u) = return (foldl (\acc (l,o,_) -> ConT o `AppT` l `AppT` acc) u ts)
    go ts (ILCons l o r) =
      do ofx <- fromMaybe defaultFixity <$> reifyFixityCompat o
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
resolveInfixT = return
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

#if MIN_VERSION_template_haskell(2,11,0)
takeFieldStrictness :: [(a,Bang,b)]   -> [FieldStrictness]
#else
takeFieldStrictness :: [(a,Strict,b)] -> [FieldStrictness]
#endif
takeFieldStrictness xs = [normalizeStrictness a | (_,a,_) <- xs]

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
     subst <- T.sequence (Map.fromList xs)
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


-- | Construct a homogeneous equality constraint. The implementation of 'Pred' varies
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


-- | Match a 'Pred' representing an equality constraint (homogeneous or
-- heterogeneous). Returns arguments to the equality constraint if successful.
asEqualPred :: Pred -> Maybe (Type,Type)
#if MIN_VERSION_template_haskell(2,10,0)
asEqualPred (EqualityT `AppT` x `AppT` y) = Just (x,y)
asEqualPred (ConT eq   `AppT` x `AppT` y)
  | eq == eqTypeName || eq == heqTypeName = Just (x,y)
#else
asEqualPred (EqualP           x        y) = Just (x,y)
#endif
asEqualPred _                             = Nothing

-- | Match a 'Pred' representing a class constraint.
-- Returns the classname and parameters if successful.
asClassPred :: Pred -> Maybe (Name, [Type])
#if MIN_VERSION_template_haskell(2,10,0)
asClassPred t =
  case decomposeType t of
    ConT f :| xs
      | f /= eqTypeName && f /= heqTypeName -> Just (f,xs)
    _                                       -> Nothing
#else
asClassPred (ClassP f xs) = Just (f,xs)
asClassPred _             = Nothing
#endif

------------------------------------------------------------------------

-- On old versions of GHC, reify would not give you kind signatures for
-- GADT type variables of kind *. To work around this, we insert the kinds
-- manually on any types without a signature.

giveTypesStarKinds :: DatatypeInfo -> DatatypeInfo
giveTypesStarKinds info =
  info { datatypeVars = annotateVars (datatypeVars info) }
  where
    annotateVars :: [Type] -> [Type]
    annotateVars = map $ \t ->
      case t of
        VarT n -> SigT (VarT n) starK
        _      -> t

giveTyVarBndrsStarKinds :: ConstructorInfo -> ConstructorInfo
giveTyVarBndrsStarKinds info =
  info { constructorVars = annotateVars (constructorVars info) }
  where
    annotateVars :: [TyVarBndr] -> [TyVarBndr]
    annotateVars = map $ \tvb ->
      case tvb of
        PlainTV n -> KindedTV n starK
        _         -> tvb

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
    (return (map ConT ds))
#else
dataDCompat = dataD
#endif

arrowKCompat :: Kind -> Kind -> Kind
#if MIN_VERSION_template_haskell(2,8,0)
arrowKCompat x y = arrowK `appK` x `appK` y
#else
arrowKCompat = arrowK
#endif

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
#if MIN_VERSION_template_haskell(2,11,0)
reifyFixityCompat n = recover (return Nothing) ((`mplus` Just defaultFixity) <$> reifyFixity n)
#else
reifyFixityCompat n = recover (return Nothing) $
  do info <- reify n
     return $! case info of
       ClassOpI _ _ _ fixity -> Just fixity
       DataConI _ _ _ fixity -> Just fixity
       VarI     _ _ _ fixity -> Just fixity
       _                     -> Nothing
#endif

-- | Call 'reify' with an action to take if reification fails.
reifyRecover ::
  Name ->
  Q Info {- ^ handle failure -} ->
  Q Info
reifyRecover n failure = failure `recover` reify n
