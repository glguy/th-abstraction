{-# LANGUAGE CPP #-}

#if MIN_VERSION_template_haskell(2,12,0)
{-# Language Safe #-}
#else
{-# Language Trustworthy #-}
#endif

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
#if MIN_VERSION_base(4,13,0)
eqTypeName = mkNameG_tc "ghc-prim" "GHC.Types" "~"
#else
eqTypeName = mkNameG_tc "base" "Data.Type.Equality" "~"
#endif
