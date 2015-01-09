{-|

`exhaustive` is a library that guarantees that when building a parser, or some
other computation that produces data, /all/ possible constructors in a data type
are considered. You can think of this library as providing a symmetry to GHC's
built in @-fwarn-incomplete-patterns@ compile time warning, although this
library is stricter in that it produces compile time errors if a constructor is
omitted.

Usage of this library is intended to be straightforward, though admittedly the
types might have you think the opposite! To understand this library, an example
may be helpful.

To begin with, consider a simple data type for a "boolean expressions" language:

@
   import qualified "GHC.Generics" as GHC

   data Expr
     = ETrue
     | EFalse
     | EIf Expr Expr Expr
     deriving ('Eq', GHC.'GHC.Generic')
   instance 'Generic' Expr
@

Note that we have to make our data type an instance of both
"GHC.Generics".'GHC.Generics.Generic' /and/ "Generics.SOP".'Generic', though this only requires
boiler-plate code.

Next, we would like to build a parser for this language. Let's assume that we
have access to a @parsec@-like library, where we have one basic combinator:

* @symbol :: 'String' -> Parser 'String'@

Ordinarily, we would write our parser as

@
    parseExpr :: Parser Expr
    parseExpr = 'msum' [ETrue '<$' symbol \"True\"
                     ,EFalse '<$' symbol \"False\"
                     ,EIf '<$>' symbol \"if\" '*>' parseExpr
                          '<*>' symbol \"then\" '*>' parseExpr
                          '<*>' symbol \"else\" '*>' parseExpr
                     ]
@

However, nothing is making sure that we actually considered all constructors in
@Expr@. We could just as well write

@
    parseExpr :: Parser Expr
    parseExpr = 'msum' [ETrue '<$' symbol \"True\"
                     ,EFalse '<$' symbol \"False\"]
@

Although this is significantly less useful!

Using @exhaustive@, we can get exhaustivity checks that we are at least
considering all constructors:

@
    parseExpr :: Parser Expr
    parseExpr = 'produceFirst' '$'
      'construct' (\\f -> f '<$' symbol \"True\") ':*'
      'construct' (\\f -> f '<$' symbol \"False\") ':*'
      'construct' (\\f -> f '<$'> symbol "if" '*>' parseExpr
                         '<*>' symbol "then" '*>' parseExpr
                         '<*>' symbol "else" '*>' parseExpr) ':*'
      'Nil'
@

As you can hopefully see, @exhaustive@ requires only minimal changes to an
existing parser. Specifically, we need to:

1. Use 'produceFirst' instead of 'msum'
2. Wrap each constructor application with 'construct'.
3. Use the provided constructor function, rather than the named constructors in
the original data type.

-}
module Control.Exhaustive
  ( -- * Producing data
    -- | The following are the main entry points to the API, all providing functionality to produce data.
    produceM
  , produceFirst
  , produceAll

    -- * Constructing Data
    -- | In order to produce data, you need a way to construct it - once for each constructor in a data type.
  , construct

    -- * Re-exported
  , Generic)
  where

import Control.Applicative
import Control.Monad
import Generics.SOP
import Control.Exhaustive.Internal
