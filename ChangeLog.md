# Revision history for th-abstraction

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
