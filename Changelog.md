## Unreleased

### Other Changes

* Changed bounds of `template-haskell` to only support 2.14.0.0. This release is
  thus only compatible with GHC 8.6. If this is a problem, please open an issue
  and I can add CPP to support older GHCs.

* Increased upper-bound of `base` to < 4.13.

## 1.1.6

### Other Changes

* Increased upper-bound of `base` to < 4.12.

## 1.1.5

### Other Changes

* Increased upper-bound of `base`.

## 1.1.4

### Other Changes

* Increased upper-bound of `generics-sop` to < 0.4.

## 1.1.3

* Compile with base-4.9.

## 1.1.2

* Compile with generics-sop 0.2. Thanks to @kosmikus for this change.

## 1.1.1

* Increased the upper-bound of base to allow < 4.9 and transforms < 0.5. Now builds on
  GHC 7.10.1.

## 1.1.0

* New API new using Template Haskell to provide named constructors. Users should
  check the latest documentation for 'Control.Exhaustive' to see how the new API
  is used.

## 1.0.0

* Initial release
