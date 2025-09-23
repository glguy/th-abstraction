{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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

#if MIN_VERSION_base(4,13,0)
import Data.Type.Equality
#endif

eqTypeName :: Name
#if MIN_VERSION_base(4,13,0)
eqTypeName = ''(~)
#else
eqTypeName = mkNameG_tc "base" "Data.Type.Equality" "~"
#endif
