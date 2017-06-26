# Revision history for th-abstraction

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
