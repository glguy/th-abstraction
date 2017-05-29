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
 , 'datatypeVars'    = [ 'SigT' ('VarT' a_3530822107858468866) 'StarT' ]
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
  , asEqualPred
  , asClassPred

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
import           Data.Maybe
import           Control.Monad (foldM)
import           GHC.Generics (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype.Internal
import           Language.Haskell.TH.Lib (arrowK, starK) -- needed for th-2.4

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
  | InfixConstructor         -- ^ Constructor without field names that is
                             --   declared infix
  | RecordConstructor [Name] -- ^ Constructor with field names
  deriving (Show, Eq, Ord, Typeable, Data, Generic)


-- | Construct a Type using the datatype's type constructor and type
-- parameters. Kind signatures are removed.
datatypeType :: DatatypeInfo -> Type
datatypeType di
  = foldl AppT (ConT (datatypeName di))
  $ map stripSigT
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
normalizeInfo (DataConI name _ parent) = reifyParent name parent
# else
normalizeInfo (DataConI name _ parent _) = reifyParent name parent
# endif
normalizeInfo _ = fail "reifyDatatype: Expected a type constructor"


reifyParent :: Name -> Name -> Q DatatypeInfo
reifyParent con parent =
  do info <- reify parent
     case info of
       TyConI dec -> normalizeDec dec
       FamilyI dec instances ->
         do let instances1 = map (repairEtaReduction dec) instances
                instances2 = map (repairFamKindSigs dec) instances1
            instances3 <- traverse normalizeDec instances2
            case find p instances3 of
              Just inst -> return inst
              Nothing   -> fail "PANIC: reifyParent lost the instance"
       _ -> fail "PANIC: reifyParent unexpected parent"
  where
    p info = con `elem` map constructorName (datatypeCons info)

    -- GHC leaves off the kind signatures on the type patterns of data family
    -- instances where a kind signature isn't specified explicitly.
    -- Here, we can use the parent data family's type variable binders to
    -- reconstruct the kind signatures if they are missing.
    repairFamKindSigs :: Dec -> Dec -> Dec
    repairFamKindSigs famD instD
#if MIN_VERSION_template_haskell(2,11,0)
      | DataFamilyD _ dvars _ <- famD
      , NewtypeInstD cx n ts k con deriv <- instD
      = NewtypeInstD cx n (repairVarKindsWith dvars ts) k con deriv
      | DataFamilyD _ dvars _ <- famD
      , DataInstD cx n ts k con deriv <- instD
      = DataInstD cx n (repairVarKindsWith dvars ts) k con deriv
#else
      | FamilyD _ _ dvars _ <- famD
      , NewtypeInstD cx n ts con deriv <- instD
      = NewtypeInstD cx n (repairVarKindsWith dvars ts) con deriv
      | FamilyD _ _ dvars _ <- famD
      , DataInstD cx n ts con deriv <- instD
      = DataInstD cx n (repairVarKindsWith dvars ts) con deriv
#endif
      | otherwise = instD
      where
        repairVarKindsWith :: [TyVarBndr] -> [Type] -> [Type]
        repairVarKindsWith = zipWith stealKindForType

        -- If a VarT is missing an explicit kind signature,
        -- steal it from a TyVarBndr.
        stealKindForType :: TyVarBndr -> Type -> Type
        stealKindForType tvb t@VarT{} = SigT t (tvbKind tvb)
        stealKindForType _   t        = t

#if MIN_VERSION_template_haskell(2,8,0) && (!MIN_VERSION_template_haskell(2,10,0))
    kindPart (KindedTV _ k) = [k]
    kindPart (PlainTV  _  ) = []

    etaExpandTypes :: [TyVarBndr] -> [Type] -> [Type]
    etaExpandTypes dvars ts =
      let nparams   = length dvars
          kparams   = countKindVars dvars
          tsNoKinds = drop kparams ts
          extraTys  = drop (length tsNoKinds) (bndrParams dvars)
      in tsNoKinds ++ extraTys

    countKindVars = length . freeVariables . map kindPart
    -- GHC 7.8.4 will eta-reduce data instances. We can find the missing
    -- type variables on the data constructor.
    --
    -- We opt to avoid propagating these new type variables through to the
    -- constructor now, but we will return to this task in normalizeCon.
    repairEtaReduction
      (FamilyD _ _ dvars _)
      (NewtypeInstD cx n ts con deriv) =
        NewtypeInstD cx n (etaExpandTypes dvars ts) con deriv
    repairEtaReduction
      (FamilyD _ _ dvars _)
      (DataInstD cx n ts cons deriv) =
        DataInstD cx n (etaExpandTypes dvars ts) cons deriv
#endif
    repairEtaReduction _ x = x


-- | Normalize 'Dec' for a newtype or datatype into a 'DatatypeInfo'.
-- Fail in 'Q' otherwise.
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
  do cons' <- concat <$> traverse (normalizeCon name params variant) cons
     pure DatatypeInfo
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
      mbFi <- reifyFixity n
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
      -- it is not equal to (Fixity 9 InfixL), as GHC always uses that as its
      -- default fixity. Unfortunately, there is no way to distinguish between
      -- a user-suppled (Fixity 9 InfixL) and the default (Fixity 9 InfixL),
      -- so we cannot properly handle that case.
      DataConI _ _ _ fi <- reify n
      let userSuppliedFixity = fi /= Fixity 9 InfixL
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
                    let ts = map snd xs
                    fi <- if gadt
                             then checkGadtFixity ts n
                             else return NormalConstructor
                    pure [ConstructorInfo n tyvars context ts fi]
                  InfixC l n r ->
                    pure [ConstructorInfo n tyvars context [snd l,snd r]
                                          InfixConstructor]
                  RecC n xs ->
                    let fns = takeFieldNames xs in
                    pure [ConstructorInfo n tyvars context
                            (takeFieldTypes xs) (RecordConstructor fns)]
                  ForallC tyvars' context' c' ->
                    go (tyvars'++tyvars) (context'++context) True c'
#if MIN_VERSION_template_haskell(2,11,0)
                  GadtC ns xs innerType ->
                    let ts = map snd xs in
                    gadtCase ns innerType ts (checkGadtFixity ts)
                  RecGadtC ns xs innerType ->
                    let fns = takeFieldNames xs in
                    gadtCase ns innerType (takeFieldTypes xs)
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
                  NormalC n _ ->
                    dataFamCase' n tyvars NormalConstructor
                  InfixC _ n _ ->
                    dataFamCase' n tyvars InfixConstructor
                  RecC n xs ->
                    dataFamCase' n tyvars (RecordConstructor (takeFieldNames xs))
                  ForallC tyvars' context' c' ->
                    go (tyvars'++tyvars) c'

          dataFamCase' :: Name -> [TyVarBndr] -> ConstructorVariant
                       -> Q [ConstructorInfo]
          dataFamCase' n tyvars variant = do
            info <- reify n
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
                               returnTy' argTys (const $ return variant)
              _ -> fail "normalizeCon: impossible"

          {-
          isRawVar :: Type -> Bool
          isRawVar VarT{}     = True
          isRawVar (SigT t _) = isRawVar t
          isRawVar _          = False

          -- Count the number of times a predicate is true
          count :: (a -> Bool) -> [a] -> Int
          count _ [] = 0
          count p (x:xs) | p x       = 1 + count p xs
                         | otherwise = count p xs
          -}

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
      in case variant of
           -- On GHC 7.6 and 7.8, there's quite a bit of post-processing that
           -- needs to be performed to work around an old bug that eta-reduces the
           -- type patterns of data families.
           DataInstance    -> dataFamCompatCase
           NewtypeInstance -> dataFamCompatCase
           Datatype	       -> defaultCase
           Newtype         -> defaultCase
#else
      in defaultCase
#endif

normalizeGadtC ::
  Name        {- ^ Type constructor             -} ->
  [Type]      {- ^ Type parameters              -} ->
  [TyVarBndr] {- ^ Constructor parameters       -} ->
  Cxt         {- ^ Constructor context          -} ->
  [Name]      {- ^ Constructor names            -} ->
  Type        {- ^ Declared type of constructor -} ->
  [Type]      {- ^ Constructor field types      -} ->
  (Name -> Q ConstructorVariant)
              {- ^ Determine a constructor variant
                   from its 'Name' -}              ->
  Q [ConstructorInfo]
normalizeGadtC typename params tyvars context names innerType fields getVariant =
  do innerType' <- resolveTypeSynonyms innerType
     case decomposeType innerType' of
       ConT innerTyCon :| ts | typename == innerTyCon ->

         let (substName, context1) = mergeArguments params ts
             subst   = VarT <$> substName
             tyvars' = [ tv | tv <- tyvars, Map.notMember (tvName tv) subst ]

             context2 = applySubstitution subst (context1 ++ context)
             fields'  = applySubstitution subst fields
         in sequence [ ConstructorInfo name tyvars' context2 fields' <$> variantQ
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
data NonEmptySnoc a = [a] :|- a

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


-- | Match a 'Pred' representing an equality constraint. Returns
-- arguments to the equality constraint if successful.
asEqualPred :: Pred -> Maybe (Type,Type)
#if MIN_VERSION_template_haskell(2,10,0)
asEqualPred (EqualityT `AppT` x `AppT` y)                    = Just (x,y)
asEqualPred (ConT eq   `AppT` x `AppT` y) | eq == eqTypeName = Just (x,y)
#else
asEqualPred (EqualP            x        y)                   = Just (x,y)
#endif
asEqualPred _                                                = Nothing

-- | Match a 'Pred' representing a class constraint.
-- Returns the classname and parameters if successful.
asClassPred :: Pred -> Maybe (Name, [Type])
#if MIN_VERSION_template_haskell(2,10,0)
asClassPred t =
  case decomposeType t of
    ConT f :| xs | f /= eqTypeName -> Just (f,xs)
    _                              -> Nothing
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
