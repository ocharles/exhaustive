{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
    'makeExhaustive' ''Expr

    parseExpr :: Parser Expr
    parseExpr =
      'produceFirst' '$'
        $('con' 'ETrue) '<$' symbol \"True\" '&:'
        $('con' 'EFalse) '<$' symbol \"False\" '&:'
        $('con' 'EIf) '<$>' (symbol \"if\" '*>' parseExpr)
                    '<*>' (symbol \"then\" '*>' parseExpr)
                    '<*>' (symbol \"else\" '*>' parseExpr) '&:'
        'finish'
@

As you can hopefully see, @exhaustive@ requires only minimal changes to an
existing parser. Specifically, we need to:

1. Use 'produceFirst' instead of 'msum'
2. Wrap each constructor application with the Template Haskell function
'con'. Note that you also need to quote the name of the constructor with a
single @'@.
3. Use '&:' to combine constructors, rather than list notation.
4. Explicitly state you are 'finish'ed.
5. Add a call to 'makeExhaustive' on our original data type.

-}

module Control.Exhaustive
       (-- * Specifying Individual Constructions
        con,
        -- * Combining Constructions
        (&:), finish,
        -- * Producing Data
        produceM, produceFirst, produceAll,
        -- * Utilities
        makeExhaustive,
        -- * Implementation details
        -- | The following are implementation details, but exported to improve documentation.
        ConstructorApplication, Construction,
        Length)
       where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.Traversable
import GHC.TypeLits (Nat, type (+))
import Generics.SOP
import Generics.SOP.NP
import Language.Haskell.TH
import Prelude hiding (foldr, sequence)

-- | Compute the length of a type level list.
type family Length (a :: [k]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

-- | A 'Construction' is an internal representation of a data type constructor. This type
-- is indexed by a natural number, which represents the constructor number,
-- and the list of types of fields of this constructor.
--
-- To create a 'Construction', use 'con'.
data Construction :: Nat -> [*] -> * where
  Construction :: NP I xs -> Construction n xs

-- | A 'ConstructorApplication' is a lifted function (in the terms of @generics-sop@) that
-- instantiates a particular constructor of a data type, possibly using
-- the side-effects provided by @f@.
--
-- To create and use 'ConstructorApplication's, use '&:'.
type ConstructorApplication f code = Injection (NP I) code -.-> K (f (NS (NP I) code))

name :: Con -> Name
name (NormalC n _) = n
name (RecC n _) = n
name (InfixC _ n _) = n
name (ForallC _ _ c) = name c

conFields :: Con -> [Type]
conFields (NormalC _ f) = map snd f
conFields (RecC _ f) = map (\(_, _, t) -> t) f
conFields (InfixC l _ r) = map snd [l,r]
conFields (ForallC _ _ c) = conFields c

typeVars :: [Type] -> [Name]
typeVars [] = []
typeVars (VarT v : vs) = v : typeVars vs
typeVars (_ : vs) = typeVars vs

parentName :: Info -> Maybe ParentName
parentName (DataConI _ _ parent _) = Just parent
parentName _ = Nothing

constructors :: Name -> Q (Maybe [Con])
constructors t =
  do info <- reify t
     case info of
       TyConI (DataD _ _ _ ctors _) ->
         return (Just ctors)
       TyConI (NewtypeD _ _ _ ctor _) ->
         return (Just [ctor])
       TyConI (DataInstD _ _ _ ctors _) ->
         return (Just ctors)
       TyConI (NewtypeInstD _ _ _ ctor _) ->
         return (Just [ctor])
       _ -> return Nothing


-- | 'con' builds a 'Construction' for a single constructor of a data type.
-- Unfortunately, as this function is used via Template Haskell, the type
-- is not particularly informative -- though you can think of the produced
-- function having roughly the same type as the original constructor.
-- To clarify this, it's helpful to look at the type of 'con' applications:
--
-- @
--     $('con' \''Nothing') :: Construction 1 '[]
--     $('con' \''Just') :: a -> Construction 2 '[a]
--
--     data Record = Record { a :: String, b :: Int, c :: Char }
--     $('con' \'Record) :: String -> Int -> Char -> Construction 1 '[String, Int, Char]
-- @
--
-- For more examples of 'con', see the module documentation at the top of this page.
con :: Name -> Q Exp
con ctorName =
  do info <- reify ctorName
     parent <- maybe (fail (show ctorName ++ " is not a data type constructor"))
                     return
                     (parentName info)
     ctors <- maybe (fail ("Unable to determine constructors of " ++ show parent)) return =<<
              constructors parent
     let matching =
           filter ((ctorName ==) . name . snd)
                  (zip [0 ..] ctors)
     case matching of
       [] ->
         fail ("Failed to find constructor index of " ++ show ctorName)
       ((i,c):_) ->
         let fieldTypes = conFields c
             lambda =
               (do names <- sequence ((newName "x") <$
                                      fieldTypes)
                   return (LamE (VarP <$> names)
                                (AppE (ConE 'Construction)
                                      (foldr (\x y ->
                                                InfixE (Just x)
                                                       (ConE '(:*))
                                                       (Just y))
                                             (ConE 'Nil)
                                             (map (AppE (ConE 'I) .
                                                   VarE)
                                                  names)))))
             lType =
               (pure (ForallT (map PlainTV (typeVars fieldTypes))
                              []
                              (foldr (\l r ->
                                        AppT (AppT ArrowT l) r)
                                     (AppT (AppT (ConT ''Construction)
                                                 (LitT (NumTyLit (succ i))))
                                           (foldr (\l r ->
                                                     AppT (AppT PromotedConsT l) r)
                                                  PromotedNilT
                                                  fieldTypes))
                                     fieldTypes)))
         in sigE lambda lType


-- | Signify that you will be performing exhaustive construction of a specific data type:
--
-- @
--     data Expr = ETrue | EFalse
--     makeExhaustive ''Expr
-- @
--
-- 'makeExhaustive' doesn't introduce any new symbols into scope, but it forces an
-- environment change, allowing you to write @$(con 'ETrue)@. If you are already using
-- other Template Haskell routines (such as @makeLenses@) then you can omit this call.
makeExhaustive :: Name -> Q [a]
makeExhaustive _ = return []

infixr 3 &:

-- | Combine multiple 'Construction's into a list of constructions for a data
-- type. This function is a lot like ':' for lists, but the types carry
-- considerably more information.
--
-- The type @n@ is used to carry the index of the constructor in the list of
-- constructors in the data type, while @xs@ is a list of types that are the
-- fields of that constructor.
--
-- The constraint on this function forces '&:' to be used to produce in-order
-- constructors. It may help to see this function through an example:
--
-- Given @data Bool = True | False@, we have two constructors. @True@ has index
-- 1, while the /code/ for this data type has length 2 (as there are two
-- constructors in total). Therefore after using the @True@ constructor we have to
-- use one more constructor. When we construct using @False@ we are done, as the
-- only way to satisfy the equation @2 + x = 2@ is to provide @x = 0@ -- the empty
-- list.
(&:) :: (Functor f, Length code ~ (n + Length xs))
     => f (Construction n x) -> NP (ConstructorApplication f code) xs -> NP (ConstructorApplication f code) (x ': xs)
(&:) f xs = construct f :* xs
  where construct constructed =
          Fn (\(Fn inject) ->
                (K (fmap (unK . inject . fields) constructed)))
        fields (Construction a) = a

-- | Assert that you have now used all constructors and are finished. If you've
-- made mistake, be prepared for a rather impressive type error!
finish :: NP f '[]
finish = Nil

-- | Keep attempting to construct a data type until a constructor succeeds. The
-- first constructor to successfully be constructed (in the order defined in the
-- original data type) will be returned, or 'empty' if all constructions fail.
produceFirst
  :: (code ~ Code a, SingI code, Generic a, Alternative f)
  => NP (ConstructorApplication f code) code -> f a
produceFirst = asum . produceM

-- | Produce all successful constructions of a data-type. If any constructors
-- fail, they will not be included in the resulting list. If all constructors
-- fail, this will return 'pure' @[]@.
produceAll
  :: (code ~ Code a, SingI code, Generic a, Alternative f)
  => NP (ConstructorApplication f code) code -> f [a]
produceAll = fmap catMaybes . sequenceA . map optional . produceM

-- | Build a list of computations, one for each constructor in a data type.
produceM
  :: (code ~ Code a, SingI code, Generic a, Applicative f)
  => NP (ConstructorApplication f code) code
  -> [f a]
produceM fs =
  map (fmap (to . SOP))
            (collapse_NP (fs `hap` injections))
