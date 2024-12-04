# Revision history for th-abstraction

## next -- ????.??.??
* Drop support for pre-8.0 versions of GHC.

## 0.7.0.0 -- 2024.03.17
* `DatatypeInfo` now has an additional `datatypeReturnKind` field. Most of the
  time, this will be `StarT`, but this can also be more exotic kinds such as
  `ConT ''UnliftedType` if dealing with primitive types, `UnliftedDatatypes`,
  or `UnliftedNewtypes`.
* `reifyDatatype` and related functions now support primitive types such as
  `Int#`. These will be reified as `DatatypeInfo`s with no `ConstructorInfo`s
  and with `Datatype` as the `datatypeVariant`.
* `normalizeCon` now takes a `Kind` argument representing the return kind of
  the parent data type. (This is sometimes necessary to determine which type
  variables in the data constructor are universal or existential, depending
  on if the variables appear in the return kind.)
* Fix a couple of bugs in which `normalizeDec` would return incorrect results
  for GADTs that use `forall`s in their return kind.

## 0.6.0.0 -- 2023.07.31
* Support building with `template-haskell-2.21.0.0` (GHC 9.8).
* Adapt to `TyVarBndr`s for type-level declarations changing their type from
  `TyVarBndr ()` to `TyVarBndr BndrVis` in `template-haskell`:

  * `Language.Haskell.TH.Datatype.TyVarBndr` now backports `type BndrVis = ()`,
    as well as `BndrReq` and `BndrInvis` pattern synonyms. These make it
    possible to write code involving `BndrVis` that is somewhat backwards
    compatible (but do see the caveats in the Haddocks for `BndrInvis`).
  * `Language.Haskell.TH.Datatype.TyVarBndr` also backports the following
    definitions:
    * The `type TyVarBndrVis = TyVarBndr BndrVis` type synonym.
    * The `DefaultBndrFlag` class, which can be used to write code that is
      polymorphic over `TyVarBndr` flags while still allowing the code to return
      a reasonable default value for the flag.
    * The `bndrReq` and `bndrInvis` definitions, which behave identically to
      `BndrReq` and `BndrInvis`.
  * `Language.Haskell.TH.Datatype.TyVarBndr` now defines the following utility
    functions, which are not present in `template-haskell`:
    * `plainTVReq`, `plainTVInvis`, `kindedTVReq`, and `kindedTVInvis`
      functions, which construct `PlainTV`s and `KindedTV`s with particular
      `BndrVis` flags.
    * An `elimTVFlag`, which behaves like `elimTV`, but where the continuation
      arguments also take a `flag` argument. (Note that the type of this
      function is slightly different on old versions of `template-haskell`.
      See the Haddocks for more.)
    * A `tvFlag` function, which extracts the `flag` from a `TyVarBndr`. (Note
      that the type of this function is slightly different on old versions of
      `template-haskell`. See the Haddocks for more.)
  * The types of the `dataDCompat` and `newtypeDCompat` functions have had
    their `[TyVarBndrUnit]` arguments changed to `[TyVarBndrVis]`, matching
    similar changes to `DataD` and `NewtypeD` in `template-haskell`.

  Because `BndrVis` is a synonym for `()` on pre-9.8 versions of GHC, this
  change is unlikely to break any existing code, provided that you build it
  with GHC 9.6 or earlier. If you build with GHC 9.8 or later, on the other
  hand, it is likely that you will need to update your existing code. Here are
  some possible ways that your code might fail to compile with GHC 9.8, along
  with some migration strategies:

  * Your code passes a `TyVarBndrUnit` in a place where a `TyVarBndrVis` is now
    expected in GHC 9.8, such as in the arguments to `dataDCompat`:

    ```hs
    import "template-haskell" Language.Haskell.TH
    import "th-abstraction"   Language.Haskell.TH.Datatype (dataDCompat)

    dec :: DecQ
    dec = dataDCompat (pure []) d [PlainTV a ()] [] []
      where
        d = mkName "d"
        a = mkName "a"
    ```

    With GHC 9.8, this will fail to compile with:

    ```
    error: [GHC-83865]
        • Couldn't match expected type ‘BndrVis’ with actual type ‘()’
        • In the second argument of ‘PlainTV’, namely ‘()’
          In the expression: PlainTV a ()
          In the third argument of ‘dataDCompat’, namely ‘[PlainTV a ()]’
      |
      | dec = dataDCompat (pure []) d [PlainTV a ()] [] []
      |                                          ^^
    ```

    Some possible ways to migrate this code include:

    * Use the `bndrReq` function or `BndrReq` pattern synonym in place of `()`,
      making sure to import them from `Language.Haskell.TH.Datatype.TyVarBndr`:

      ```hs
      ...
      import "th-abstraction" Language.Haskell.TH.Datatype.TyVarBndr

      dec :: DecQ
      dec = dataDCompat (pure []) d [PlainTV a bndrReq] [] []
      -- Or, alternatively:
      {-
      dec = dataDCompat (pure []) d [PlainTV a BndrReq] [] []
      -}
        where
          ...
      ```
    * Use the `plainTV` function from `Language.Haskell.TH.Datatype.TyVarBndr`,
      which is now sufficiently polymorphic to work as both a `TyVarBndrUnit`
      and a `TyVarBndrVis`:

      ```hs
      ...
      import Language.Haskell.TH.Datatype.TyVarBndr

      dec :: DecQ
      dec = dataDCompat (pure []) d [plainTV a] [] []
        where
          ...
      ```
  * You may have to replace some uses of `TyVarBndrUnit` with `TyVarBndrVis`
    in your code. For instance, this will no longer typecheck in GHC 9.8 for
    similar reasons to the previous example:

    ```hs
    import "template-haskell" Language.Haskell.TH
    import "th-abstraction"   Language.Haskell.TH.Datatype (dataDCompat)

    dec :: DecQ
    dec = dataDCompat (pure []) d tvbs [] []
      where
        tvbs :: [TyVarBndrUnit]
        tvbs = [plainTV a]

        d = mkName "d"
        a = mkName "a"
    ```

    Here is a version that will typecheck with GHC 9.8 and earlier:

    ```hs
    ...
    import "th-abstraction" Language.Haskell.TH.Datatype.TyVarBndr

    dec :: DecQ
    dec = dataDCompat (pure []) d tvbs [] []
      where
        tvbs :: [TyVarBndrVis]
        tvbs = [plainTV a]

        ...
    ```
  * In some cases, the `TyVarBndrUnit`s might come from another place in the
    code, e.g.,

    ```hs
    import "template-haskell" Language.Haskell.TH
    import "th-abstraction"   Language.Haskell.TH.Datatype (dataDCompat)

    dec :: [TyVarBndrUnit] -> DecQ
    dec tvbs = dataDCompat (pure []) d tvbs [] []
      where
        d = mkName "d"
    ```

    If it is not straightforward to change `dec`'s type to accept
    `[TyVarBndrVis]` as an argument, another viable option is to use the
    `changeTVFlags` function:

    ```hs
    ...
    import "th-abstraction" Language.Haskell.TH.Datatype.TyVarBndr

    dec :: [TyVarBndrUnit] -> DecQ
    dec tvbs = dataDCompat (pure []) d tvbs' [] []
      where
        tvbs' :: [TyVarBndrVis]
        tvbs' = changeTVFlags bndrReq tvbs

        ...
    ```

  This guide, while not comprehensive, should cover most of the common cases one
  will encounter when migrating their `th-abstraction` code to support GHC 9.8.

## 0.5.0.0 -- 2023.02.27
* Support the `TypeData` language extension added in GHC 9.6. The
  `DatatypeVariant` data type now has a separate `TypeData` constructor to
  represent `type data` declarations.
* Add a `Lift` instance for `th-abstraction`'s compatibility shim for
  `Specificity` when building with pre-9.0 versions of GHC.

## 0.4.5.0 -- 2022.09.12
* Fix a bug in which data family declarations with interesting return kinds
  (e.g., `data family F :: Type -> Type`) would be reified incorrectly when
  using `reifyDatatype`.

## 0.4.4.0 -- 2022.07.23
* Support free variable substitution and infix resolution for
  `PromotedInfixT` and `PromotedUInfixT` on `template-haskell-2.19.0.0` or
  later.

## 0.4.3.0 -- 2021.08.30
* Make `applySubstitution` avoid capturing type variable binders when
  substituting into `forall`s.
* Fix a bug in which `resolveTypeSynonyms` would incorrectly expand type
  synonyms that are not applied to enough arguments.
* Allow the test suite to build with GHC 9.2.

## 0.4.2.0 -- 2020-12-30
* Explicitly mark modules as Safe (or Trustworthy for GHC versions prior to 8.4).

## 0.4.1.0 -- 2020-12-09
* Fix a bug in which `normalizeDec` would give incorrect kind annotations to
  type variables in quoted `Dec`s. `normalizeDec` now leaves the kinds of
  type variable binders alone.

## 0.4.0.0 -- 2020-09-29
* Adapt to the `TyVarBndr` data type gaining a new `flag` type parameter
  (in `template-haskell-2.17.0.0`) to represent its specificity:
  * Introduce a new `Language.Haskell.TH.Datatype.TyVarBndr` module that
    defines `TyVarBndr_`, a backwards-compatible type synonym for `TyVarBndr`,
    as well as backporting `TyVarBndrSpec`, `TyVarBndrUnit`, and `Specificity`.
    This module also defines other useful functions for constructing and
    manipulating `TyVarBndr`s.
  * The types in `Language.Haskell.TH.Datatype` now use `TyVarBndr_`,
    `TyVarBndrUnit`, and `TyVarBndrSpec` where appropriate. Technically, this
    is not a breaking change, since all three are simple type synonyms around
    `TyVarBndr`, but it is likely that you will need to update your
    `th-abstraction`-using code anyway if it involves a `TyVarBndr`-consuming
    function.

## 0.3.2.0 -- 2020-02-06
* Support substituting into and extracting free variables from `ForallVisT`s
  on `template-haskell-2.16.0.0` (GHC 8.10) or later.
* Fix a bug in which `freeVariables` could report duplicate kind variables when
  they occur in the kinds of the type variable binders in a `ForallT`.
* Fix a bug in which `resolveInfixT` would not resolve `UInfixT`s occurring in
  the kinds of type variable binders in a `ForallT`.
* Fix a bug in which the `TypeSubstitution ConstructorInfo` instance would not
  detect free kind variables in the `constructorVars`.

## 0.3.1.0 -- 2019-04-28
* Fix a bug which would cause data family information to be reified incorrectly
  with GHC 8.8+ in some situations.

## 0.3.0.0 -- 2019-04-26
* Breaking change: the `datatypeVars` field of `DatatypeInfo` is now of type
  `[TyVarBndr]` instead of `[Type]`, as it now refers to all of the bound type
  variables in the data type. The old `datatypeVars` field has been renamed to
  `datatypeInstTypes` to better reflect its purpose.

  In addition, the type of `normalizeCon` now has an additional `[TyVarBndr]`
  argument, since `DatatypeInfo` now requires it.
* Support `template-haskell-2.15`.
* Fix a bug in which `normalizeDec` would not detect existential type variables
  in a GADT constructor if they were implicitly quantified.
* Fix a bug in which `normalizeDec` would report an incorrect number of
  `datatypeVars` for GADT declarations with explicit return kinds (such as
  `data Foo :: * -> * where`).

## 0.2.11.0 -- 2019-02-26
* Fix a bug in which `freeVariablesWellScoped` would sometimes not preserve
  the left-to-right ordering of `Name`s generated with `newName`.

## 0.2.10.0 -- 2018-12-20
* Optimization: `quantifyType` now collapses consecutive `forall`s. For
  instance, calling `quantifyType` on `forall b. a -> b -> T a` now produces
  `forall a b. a -> b -> T a` instead of `forall a. forall b. a -> b -> T a`.

## 0.2.9.0 -- 2018-12-20
* Fix a bug in which `resolveTypeSynonyms` would not look into `ForallT`s,
  `SigT`s, `InfixT`s, or `ParensT`s.
* Fix a bug in which `quantifyType` would not respect the dependency order of
  type variables (e.g., `Proxy (a :: k)` would have erroneously been quantified
  as `forall a k. Proxy (a :: k)`).
* Fix a bug in which `asEqualPred` would return incorrect results with GHC 8.7.
* Add a `freeVariablesWellScoped` function which computes the free variables of
  a list of types and sorts them according to dependency order.
* Add a `resolveKindSynonyms` function which expands all type synonyms in a
  `Kind`. This is mostly useful for supporting old GHCs where `Type` and `Kind`
  were not the same.

## 0.2.8.0 -- 2018-06-29
* GADT reification is now much more robust with respect to `PolyKinds`:
  * A bug in which universally quantified kind variables were mistakenly
    flagged as existential has been fixed.
  * A bug in which the kinds of existentially quantified type variables
    were not substituted properly has been fixed.
  * More kind equalities are detected than before. For example, in the
    following data type:

    ```haskell
    data T (a :: k) where
      MkT :: forall (a :: Bool). T a
    ```

    We now catch the `k ~ Bool` equality.
* Tweak `resolveTypeSynonyms` so that failing to reify a type constructor
  name so longer results in an error. Among other benefits, this makes
  it possible to pass data types with GADT syntax to `normalizeDec`.

## 0.2.7.0 -- 2018-06-17
* Fix bug in which data family instances with duplicate occurrences of type
  variables in the left-hand side would have redundant equality constraints
  in their contexts.

## 0.2.6.0 -- 2017-09-04
* Fix bug in which `applySubstitution` and `freeVariables` would ignore
  type variables in the kinds of type variable binders.

## 0.2.5.0

* Added `pragLineDCompat`, `newtypeDCompat` and `tySynInstDCompat`

## 0.2.4.0 -- 2017-07-31

* Fix bug that caused GADT equality constraints to be incorrect in some cases.
* Expose `Unpackedness` and `Strictness` (which were unexported by accident).

## 0.2.3.0 -- 2017-06-26

* Add `resolvePredSynonyms`
* Add `reifyConstructor`, which allows reification of `ConstructorInfo` from
  a constructor name, and `lookupByConstructorName`, which allows directly
  looking up a `ConstructorInfo` from a `DatatypeInfo` value for a given
  constructor `Name`.
* Augment `reifyDatatype` to be able to look up `DatatypeInfo` from the `Name`
  of a record selector for one of its constructors. Also add `reifyRecord` for
  reification of of `ConstructorInfo` from a record name, and
  `lookupByRecordName`, which allows directly looking up a `ConstructorInfo`
  from a `DatatypeInfo` value for a given record `Name`.
* Fix bug that caused `th-abstraction` to fail on GHC 7.0 and 7.2 when passing
  a vanilla constructor name to `reifyDatatype`
* Make `normalizeDec` and `normalizeCon` more robust with respect to
  data family instances on GHC 7.6 and 7.8

## 0.2.2.0 -- 2017-06-10

* Fix `freeVariables` on lists not not produce duplicates.

## 0.2.1.0 -- 2017-06-09

* Add sensible reify defaults and error messages when we
  can't backport fixes to old GHC Template Haskell output
  due to hand-written Decs being processed.

## 0.2.0.0 -- 2017-06-03

* Added `reifyFixityCompat`
* Added `constructorStrictness` field to `ConstructorInfo`
* Infer more kind signatures when missing on old GHCs
* Added parameter to `normalizeCon`
* Support GHC back to 7.0.4

## 0.1.3.0 -- 2017-05-27

* Added `resolveInfixT` which uses reified fixity information to resolve `UInfixT`
* Added `asEqualPred` and `asClassPred`
* Fixed data-instance GADTs

## 0.1.2.1  -- 2017-05-21

* Add eta reduction fixes to GHC 7.6

## 0.1.2.0  -- 2017-05-21

* Added `arrowKCompat`
* Added workaround for GHC 7.8 data instance eta reduction bug
* Added kind signatures to datatypeVars

## 0.1.1.0  -- 2017-05-20

* Better matching of constraints generated for GADTs across GHC versions
* Added `dataDCompat`
* Support for giving value constructors to reifyDatatype. This enables
  data families to be reified easily.

## 0.1.0.0  -- 2017-04-26

* First version.
