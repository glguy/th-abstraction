{-# Language CPP, DeriveDataTypeable #-}

#if MIN_VERSION_base(4,4,0)
#define HAS_GENERICS
{-# Language DeriveGeneric #-}
#endif

#if MIN_VERSION_template_haskell(2,12,0)
{-# Language Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# Language Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
#define HAS_TH_LIFT
{-# Language DeriveLift #-}
#endif

{-|
Module      : Language.Haskell.TH.Datatype.TyVarBndr
Description : Backwards-compatible type variable binders
Copyright   : Eric Mertens 2020
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a backwards-compatible API for constructing and
manipulating 'TyVarBndr's across multiple versions of the @template-haskell@
package.

-}
module Language.Haskell.TH.Datatype.TyVarBndr (
    -- * @TyVarBndr@-related types
    TyVarBndr_
  , TyVarBndrUnit
  , TyVarBndrSpec
  , TyVarBndrVis
  , Specificity(..)
#if __GLASGOW_HASKELL__ >= 907
  , BndrVis(..)
#elif __GLASGOW_HASKELL__ >= 708
  , BndrVis
  , pattern BndrReq
  , pattern BndrInvis
#else
  , BndrVis
#endif
  , DefaultBndrFlag(..)

    -- * Constructing @TyVarBndr@s
    -- ** @flag@-polymorphic
  , plainTVFlag
  , kindedTVFlag
    -- ** @TyVarBndrUnit@
  , plainTV
  , kindedTV
    -- ** @TyVarBndrSpec@
  , plainTVInferred
  , plainTVSpecified
  , kindedTVInferred
  , kindedTVSpecified
    -- ** @TyVarBndrVis@
  , plainTVReq
  , plainTVInvis
  , kindedTVReq
  , kindedTVInvis

    -- * Constructing @Specificity@
  , inferredSpec
  , specifiedSpec

    -- * Constructing @BndrVis@
  , bndrReq
  , bndrInvis

    -- * Modifying @TyVarBndr@s
  , elimTV
  , elimTVFlag
  , mapTV
  , mapTVName
  , mapTVFlag
  , mapTVKind
  , traverseTV
  , traverseTVName
  , traverseTVFlag
  , traverseTVKind
  , mapMTV
  , mapMTVName
  , mapMTVFlag
  , mapMTVKind
  , changeTVFlags

    -- * Properties of @TyVarBndr@s
  , tvName
  , tvKind
  , tvFlag
  ) where

import Control.Applicative
import Control.Monad
import Data.Data (Typeable, Data)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

#ifdef HAS_GENERICS
import GHC.Generics (Generic)
#endif

-- | A type synonym for 'TyVarBndr'. This is the recommended way to refer to
-- 'TyVarBndr's if you wish to achieve backwards compatibility with older
-- versions of @template-haskell@, where 'TyVarBndr' lacked a @flag@ type
-- parameter (if it has one).
#if MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr_ flag = TyVarBndr flag
#else
type TyVarBndr_ flag = TyVarBndr

-- | A 'TyVarBndr' without a flag. This is used for 'TyVarBndr's that do not
-- interact with visible type application and are not binders for type-level
-- declarations.
type TyVarBndrUnit = TyVarBndr

-- | A 'TyVarBndr' with a 'Specificity' flag. This is used for 'TyVarBndr's that
-- interact with visible type application.
type TyVarBndrSpec = TyVarBndr

-- | Determines how a 'TyVarBndr' interacts with visible type application.
data Specificity
  = SpecifiedSpec -- ^ @a@. Eligible for visible type application.
  | InferredSpec  -- ^ @{a}@. Not eligible for visible type application.
  deriving (Show, Eq, Ord, Typeable, Data
#ifdef HAS_GENERICS
           ,Generic
#endif
#ifdef HAS_TH_LIFT
           ,Lift
#endif
           )

inferredSpec :: Specificity
inferredSpec = InferredSpec

specifiedSpec :: Specificity
specifiedSpec = SpecifiedSpec
#endif

#if !MIN_VERSION_template_haskell(2,21,0)
-- | A 'TyVarBndr' with a 'BndrVis' flag. This is used for 'TyVarBndr's in
-- type-level declarations (e.g., the binders in @data D \@k (a :: k)@).
type TyVarBndrVis = TyVarBndr_ BndrVis

-- | Because pre-9.8 GHCs do not support invisible binders in type-level
-- declarations, we simply make 'BndrVis' an alias for @()@ as a compatibility
-- shim for old GHCs. This matches how type-level 'TyVarBndr's were flagged
-- prior to GHC 9.8.
type BndrVis = ()
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE BndrReq, BndrInvis #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
-- | Because pre-9.8 GHCs do not support invisible binders in type-level
-- declarations, we simply make 'BndrReq' a pattern synonym for @()@ as a
-- compatibility shim for old GHCs. This matches how type-level 'TyVarBndr's
-- were flagged prior to GHC 9.8.
#if __GLASGOW_HASKELL__ >= 800
pattern BndrReq :: BndrVis
#endif
pattern BndrReq = ()

-- | Because pre-9.8 GHCs do not support invisible binders in type-level
-- declarations, this compatibility shim is defined in a somewhat unusual way:
--
-- * As a pattern, 'BndrInvis' will never match on pre-9.8 GHCs. That way, if
--   you write pattern matches like this:
--
--   @
--   case flag of
--     'BndrInvis' -> ...
--     'BndrVis' -> ...
--   @
--
--   Then the first branch will never be taken on pre-9.8 GHCs.
--
-- * 'BndrInvis' is a unidirectional pattern synonym on pre-9.8 GHCs, so it
--   cannot be used as an expression on these GHC versions. This is done in an
--   effort to avoid pitfalls that could occur if 'BndrInvis' were defined like
--   so:
--
--   @
--   pattern 'BndrInvis' = ()
--   @
--
--   If this were the definition, then a user could write code involving
--   'BndrInvis' that would construct an invisible type-level binder on GHC 9.8
--   or later, but a /visible/ type-level binder on older GHCs! This would be
--   disastrous, so we prevent the user from doing such a thing.
#if __GLASGOW_HASKELL__ >= 800
pattern BndrInvis :: BndrVis
#endif
pattern BndrInvis <- ((\() -> True) -> False)
#endif

bndrReq :: BndrVis
bndrReq = ()

bndrInvis :: BndrVis
bndrInvis = ()

-- | A class characterizing reasonable default values for various 'TyVarBndr'
-- @flag@ types.
class DefaultBndrFlag flag where
  defaultBndrFlag :: flag

instance DefaultBndrFlag () where
  defaultBndrFlag = ()

instance DefaultBndrFlag Specificity where
  defaultBndrFlag = SpecifiedSpec
#endif

-- | Construct a 'PlainTV' with the given @flag@.
plainTVFlag :: Name -> flag -> TyVarBndr_ flag
#if MIN_VERSION_template_haskell(2,17,0)
plainTVFlag = PlainTV
#else
plainTVFlag n _ = PlainTV n
#endif

-- | Construct a 'PlainTV' with an 'InferredSpec'.
plainTVInferred :: Name -> TyVarBndrSpec
plainTVInferred n = plainTVFlag n InferredSpec

-- | Construct a 'PlainTV' with a 'SpecifiedSpec'.
plainTVSpecified :: Name -> TyVarBndrSpec
plainTVSpecified n = plainTVFlag n SpecifiedSpec

-- | Construct a 'PlainTV' with a 'BndrReq'.
plainTVReq :: Name -> TyVarBndrVis
plainTVReq n = plainTVFlag n bndrReq

-- | Construct a 'PlainTV' with a 'BndrInvis'.
plainTVInvis :: Name -> TyVarBndrVis
plainTVInvis n = plainTVFlag n bndrInvis

-- | Construct a 'KindedTV' with the given @flag@.
kindedTVFlag :: Name -> flag -> Kind -> TyVarBndr_ flag
#if MIN_VERSION_template_haskell(2,17,0)
kindedTVFlag = KindedTV
#else
kindedTVFlag n _ kind = KindedTV n kind
#endif

-- | Construct a 'KindedTV' with an 'InferredSpec'.
kindedTVInferred :: Name -> Kind -> TyVarBndrSpec
kindedTVInferred n k = kindedTVFlag n InferredSpec k

-- | Construct a 'KindedTV' with a 'SpecifiedSpec'.
kindedTVSpecified :: Name -> Kind -> TyVarBndrSpec
kindedTVSpecified n k = kindedTVFlag n SpecifiedSpec k

-- | Construct a 'KindedTV' with a 'BndrReq'.
kindedTVReq :: Name -> Kind -> TyVarBndrVis
kindedTVReq n k = kindedTVFlag n bndrReq k

-- | Construct a 'KindedTV' with a 'BndrInvis'.
kindedTVInvis :: Name -> Kind -> TyVarBndrVis
kindedTVInvis n k = kindedTVFlag n bndrInvis k

-- | Case analysis for a 'TyVarBndr'. If the value is a @'PlainTV' n _@, apply
-- the first function to @n@; if it is @'KindedTV' n _ k@, apply the second
-- function to @n@ and @k@.
elimTV :: (Name -> r) -> (Name -> Kind -> r) -> TyVarBndr_ flag -> r
#if MIN_VERSION_template_haskell(2,17,0)
elimTV ptv _ktv (PlainTV n _)    = ptv n
elimTV _ptv ktv (KindedTV n _ k) = ktv n k
#else
elimTV ptv _ktv (PlainTV n)    = ptv n
elimTV _ptv ktv (KindedTV n k) = ktv n k
#endif

-- | Case analysis for a 'TyVarBndr' that includes @flag@s in the continuation
-- arguments. Note that 'TyVarBndr's did not include @flag@s prior to
-- @template-haskell-2.17.0.0@, so on older versions of @template-haskell@,
-- these @flag@s instead become @()@.
#if MIN_VERSION_template_haskell(2,17,0)
elimTVFlag :: (Name -> flag -> r) -> (Name -> flag -> Kind -> r) -> TyVarBndr_ flag -> r
elimTVFlag ptv _ktv (PlainTV n flag)    = ptv n flag
elimTVFlag _ptv ktv (KindedTV n flag k) = ktv n flag k
#else
elimTVFlag :: (Name -> () -> r) -> (Name -> () -> Kind -> r) -> TyVarBndr_ flag -> r
elimTVFlag ptv _ktv (PlainTV n)    = ptv n ()
elimTVFlag _ptv ktv (KindedTV n k) = ktv n () k
#endif

-- | Map over the components of a 'TyVarBndr'.
mapTV :: (Name -> Name) -> (flag -> flag') -> (Kind -> Kind)
      -> TyVarBndr_ flag -> TyVarBndr_ flag'
#if MIN_VERSION_template_haskell(2,17,0)
mapTV fn fflag _fkind (PlainTV  n flag)      = PlainTV  (fn n) (fflag flag)
mapTV fn fflag  fkind (KindedTV n flag kind) = KindedTV (fn n) (fflag flag) (fkind kind)
#else
mapTV fn _fflag _fkind (PlainTV  n)      = PlainTV  (fn n)
mapTV fn _fflag  fkind (KindedTV n kind) = KindedTV (fn n) (fkind kind)
#endif

-- | Map over the 'Name' of a 'TyVarBndr'.
mapTVName :: (Name -> Name) -> TyVarBndr_ flag -> TyVarBndr_ flag
mapTVName fname = mapTV fname id id

-- | Map over the @flag@ of a 'TyVarBndr'.
mapTVFlag :: (flag -> flag') -> TyVarBndr_ flag -> TyVarBndr_ flag'
#if MIN_VERSION_template_haskell(2,17,0)
mapTVFlag = fmap
#else
mapTVFlag _ = id
#endif

-- | Map over the 'Kind' of a 'TyVarBndr'.
mapTVKind :: (Kind -> Kind) -> TyVarBndr_ flag -> TyVarBndr_ flag
mapTVKind fkind = mapTV id id fkind

-- | Traverse the components of a 'TyVarBndr'.
traverseTV :: Applicative f
           => (Name -> f Name) -> (flag -> f flag') -> (Kind -> f Kind)
           -> TyVarBndr_ flag -> f (TyVarBndr_ flag')
#if MIN_VERSION_template_haskell(2,17,0)
traverseTV fn fflag _fkind (PlainTV n flag) =
  liftA2 PlainTV (fn n) (fflag flag)
traverseTV fn fflag fkind (KindedTV n flag kind) =
  liftA3 KindedTV (fn n) (fflag flag) (fkind kind)
#else
traverseTV fn _fflag _fkind (PlainTV n) =
  PlainTV <$> fn n
traverseTV fn _fflag fkind (KindedTV n kind) =
  liftA2 KindedTV (fn n) (fkind kind)
#endif

-- | Traverse the 'Name' of a 'TyVarBndr'.
traverseTVName :: Functor f
               => (Name -> f Name)
               -> TyVarBndr_ flag -> f (TyVarBndr_ flag)
#if MIN_VERSION_template_haskell(2,17,0)
traverseTVName fn (PlainTV n flag) =
  (\n' -> PlainTV n' flag) <$> fn n
traverseTVName fn (KindedTV n flag kind) =
  (\n' -> KindedTV n' flag kind) <$> fn n
#else
traverseTVName fn (PlainTV n) =
  PlainTV <$> fn n
traverseTVName fn (KindedTV n kind) =
  (\n' -> KindedTV n' kind) <$> fn n
#endif

-- | Traverse the @flag@ of a 'TyVarBndr'.
traverseTVFlag :: Applicative f
               => (flag -> f flag')
               -> TyVarBndr_ flag -> f (TyVarBndr_ flag')
#if MIN_VERSION_template_haskell(2,17,0)
traverseTVFlag fflag (PlainTV n flag) =
  PlainTV n <$> fflag flag
traverseTVFlag fflag (KindedTV n flag kind) =
  (\flag' -> KindedTV n flag' kind) <$> fflag flag
#else
traverseTVFlag _ = pure
#endif

-- | Traverse the 'Kind' of a 'TyVarBndr'.
traverseTVKind :: Applicative f
               => (Kind -> f Kind)
               -> TyVarBndr_ flag -> f (TyVarBndr_ flag)
#if MIN_VERSION_template_haskell(2,17,0)
traverseTVKind _fkind tvb@PlainTV{} =
  pure tvb
traverseTVKind fkind (KindedTV n flag kind) =
  KindedTV n flag <$> fkind kind
#else
traverseTVKind _fkind tvb@PlainTV{} =
  pure tvb
traverseTVKind fkind (KindedTV n kind) =
  KindedTV n <$> fkind kind
#endif

-- | Map over the components of a 'TyVarBndr' in a monadic fashion.
--
-- This is the same as 'traverseTV', but with a 'Monad' constraint. This is
-- mainly useful for use with old versions of @base@ where 'Applicative' was
-- not a superclass of 'Monad'.
mapMTV :: Monad m
       => (Name -> m Name) -> (flag -> m flag') -> (Kind -> m Kind)
       -> TyVarBndr_ flag -> m (TyVarBndr_ flag')
#if MIN_VERSION_template_haskell(2,17,0)
mapMTV fn fflag _fkind (PlainTV n flag) =
  liftM2 PlainTV (fn n) (fflag flag)
mapMTV fn fflag fkind (KindedTV n flag kind) =
  liftM3 KindedTV (fn n) (fflag flag) (fkind kind)
#else
mapMTV fn _fflag _fkind (PlainTV n) =
  liftM PlainTV (fn n)
mapMTV fn _fflag fkind (KindedTV n kind) =
  liftM2 KindedTV (fn n) (fkind kind)
#endif

-- | Map over the 'Name' of a 'TyVarBndr' in a monadic fashion.
--
-- This is the same as 'traverseTVName', but with a 'Monad' constraint. This is
-- mainly useful for use with old versions of @base@ where 'Applicative' was
-- not a superclass of 'Monad'.
mapMTVName :: Monad m
           => (Name -> m Name)
           -> TyVarBndr_ flag -> m (TyVarBndr_ flag)
#if MIN_VERSION_template_haskell(2,17,0)
mapMTVName fn (PlainTV n flag) =
  liftM (\n' -> PlainTV n' flag) (fn n)
mapMTVName fn (KindedTV n flag kind) =
  liftM (\n' -> KindedTV n' flag kind) (fn n)
#else
mapMTVName fn (PlainTV n) =
  liftM PlainTV (fn n)
mapMTVName fn (KindedTV n kind) =
  liftM (\n' -> KindedTV n' kind) (fn n)
#endif

-- | Map over the @flag@ of a 'TyVarBndr' in a monadic fashion.
--
-- This is the same as 'traverseTVFlag', but with a 'Monad' constraint. This is
-- mainly useful for use with old versions of @base@ where 'Applicative' was
-- not a superclass of 'Monad'.
mapMTVFlag :: Monad m
           => (flag -> m flag')
           -> TyVarBndr_ flag -> m (TyVarBndr_ flag')
#if MIN_VERSION_template_haskell(2,17,0)
mapMTVFlag fflag (PlainTV n flag) =
  liftM (PlainTV n) (fflag flag)
mapMTVFlag fflag (KindedTV n flag kind) =
  liftM (\flag' -> KindedTV n flag' kind) (fflag flag)
#else
mapMTVFlag _ = return
#endif

-- | Map over the 'Kind' of a 'TyVarBndr' in a monadic fashion.
--
-- This is the same as 'traverseTVKind', but with a 'Monad' constraint. This is
-- mainly useful for use with old versions of @base@ where 'Applicative' was
-- not a superclass of 'Monad'.
mapMTVKind :: Monad m
           => (Kind -> m Kind)
           -> TyVarBndr_ flag -> m (TyVarBndr_ flag)
#if MIN_VERSION_template_haskell(2,17,0)
mapMTVKind _fkind tvb@PlainTV{} =
  return tvb
mapMTVKind fkind (KindedTV n flag kind) =
  liftM (KindedTV n flag) (fkind kind)
#else
mapMTVKind _fkind tvb@PlainTV{} =
  return tvb
mapMTVKind fkind (KindedTV n kind) =
  liftM (KindedTV n) (fkind kind)
#endif

-- | Set the flag in a list of 'TyVarBndr's. This is often useful in contexts
-- where one needs to re-use a list of 'TyVarBndr's from one flag setting to
-- another flag setting. For example, in order to re-use the 'TyVarBndr's bound
-- by a 'DataD' in a 'ForallT', one can do the following:
--
-- @
-- case x of
--   'DataD' _ _ tvbs _ _ _ ->
--     'ForallT' ('changeTVFlags' 'SpecifiedSpec' tvbs) ...
-- @
changeTVFlags :: newFlag -> [TyVarBndr_ oldFlag] -> [TyVarBndr_ newFlag]
#if MIN_VERSION_template_haskell(2,17,0)
changeTVFlags newFlag = map (newFlag <$)
#else
changeTVFlags _ = id
#endif

-- | Extract the type variable name from a 'TyVarBndr', ignoring the
-- kind signature if one exists.
tvName :: TyVarBndr_ flag -> Name
tvName = elimTV id (\n _ -> n)

-- | Extract the kind from a 'TyVarBndr'. Assumes 'PlainTV' has kind @*@.
tvKind :: TyVarBndr_ flag -> Kind
tvKind = elimTV (\_ -> starK) (\_ k -> k)

-- | Extract the @flag@ from a 'TyVarBndr'. Note that 'TyVarBndr's did not
-- include @flag@s prior to @template-haskell-2.17.0.0@, so on older versions of
-- @template-haskell@, this functions instead returns @()@.
#if MIN_VERSION_template_haskell(2,17,0)
tvFlag :: TyVarBndr_ flag -> flag
tvFlag = elimTVFlag (\_ flag -> flag) (\_ flag _ -> flag)
#else
tvFlag :: TyVarBndr_ flag -> ()
tvFlag _ = ()
#endif
