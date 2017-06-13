{-# LANGUAGE CPP #-}

{-|
Module      : Language.Haskell.TH.Datatype.Internal
Description : Backwards-compatible interface to reified information about datatypes.
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

Internal Template Haskell 'Name's.

-}
module Language.Haskell.TH.Datatype.Internal where

import Language.Haskell.TH.Syntax

eqTypeName :: Name
#if MIN_VERSION_base(4,9,0)
eqTypeName = mkNameG_tc "base" "Data.Type.Equality" "~"
#else
eqTypeName = mkNameG_tc "ghc-prim" "GHC.Types" "~"
#endif

-- This is only needed for GHC 7.6-specific bug
starKindName :: Name
starKindName = mkNameG_tc "ghc-prim" "GHC.Prim" "*"
