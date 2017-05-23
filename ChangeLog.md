# Revision history for th-abstraction

## 0.1.3.0

* Added `resolveInfixT` which uses reified fixity information to resolve `UInfixT`

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
