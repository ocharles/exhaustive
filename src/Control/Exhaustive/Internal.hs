{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Exhaustive.Internal
  (Constructor, Producer, FunctorStack, produceM, produceFirst, produceAll, construct, Apply, apply )
  where

import Data.Maybe
import Data.Traversable
import Control.Applicative
import Data.Foldable
import Generics.SOP
import Generics.SOP.NP
import Data.Functor.Compose
import Generics.SOP.Constraint

-- | Internal machinery to build n-ary functions. As a user of `exhaustive`, you
-- generally shouldn't need to worry about this type class, other than knowing
-- that the type variable @b@ will correspond to a function of all types
-- mentioned in @a@.
class Functor f => Apply f a b | f a -> b where
  apply :: f a -> b

instance Apply ((->) a) b (a -> b) where
  apply = id

instance Apply I b b where
  apply (I a) = a

instance (Apply g a b,Apply f b c) => Apply (Compose f g) a c where
  apply (Compose x) = apply (fmap apply x)

-- | A 'Producer' is a lifted function (in the terms of @generics-sop@) that
-- instantiates a particular constructor of a data type, possibly using
-- the side-effects provided by @f@.
--
-- Most users will want to create 'Producer's using the smart constructor
-- 'construct'.
type Producer f code = Injection (NP I) code -.-> K (f (NS (NP I) code))

-- | A 'Constructor' is an n-ary function from all fields of a specific
-- constructor in a data type, to a generic representation.
type Constructor fields = forall r. Apply (FunctorStack fields) (NP I fields) r => r

-- | 'construct' builds a 'Producer' for a single constructor of a data type.
-- As you can see, the type is a little scary - but there are a few main parts
-- that will interest you, while the rest are unfortunate implementation
-- details.
--
-- * @f@ is the type of functor who's side effects you can use. For example,
-- you can choose @f@ to be 'IO', @(MyEnv ->)@, or even more complex
-- monad transformer stacks.
--
-- * @fields@ is a list of types that are used in the constructor.
--
--     As an example, given the data type
--
--     @data User = User { name :: 'String', age :: 'Int'}@
--
--     then @fields@ will correspond to @['String', 'Int']@.
--
-- The 'Constructor' argument is what you use to actually create your data type.
-- A 'Constructor' is an n-ary function from all field types. Continuing the
-- example with @User@ above, we would have
--
-- @Constructor fields@ == @Text -> Int -> out@
--
-- Thus a complete call to 'construct' would be
--
-- @'construct' (\\f -> f '<$>' parseName '<*>' parseAge)@
--
-- For a complete example of how this all fits together, user's are pointed
-- to the example at the top of this page.
construct :: forall fields code f.
             (Applicative f,SingI fields)
          => (Constructor fields -> f (NP I fields)) -> Producer f code fields
construct applyCtor =
  Fn (\(Fn f) ->
        (K (fmap (unK . f)
                 (applyCtor (apply (buildF (shape :: Shape fields)))))))

-- | This type family is internal, but provides the building block for building
-- n-ary functions. Most users will probably not need to work with this
-- directly.
type family FunctorStack (args :: [*]) :: * -> * where
  FunctorStack '[] = I
  FunctorStack (a ': as) = Compose ((->) a) (FunctorStack as)

data Dict (k :: Constraint) where
  Dict :: k => Dict k

-- | Prove that a 'FunctorStack' really is a 'Functor'.
isAFunctor :: Shape xs -> Dict (Functor (FunctorStack xs))
isAFunctor ShapeNil = Dict
isAFunctor (ShapeCons s) = case isAFunctor s of Dict -> Dict

-- | Given a list of types, build a stack of reader functors that represents
-- an n-ary function of all of those types.
buildF :: Functor (FunctorStack xs) => Shape xs -> FunctorStack xs (NP I xs)
buildF ShapeNil = I Nil
buildF (ShapeCons s) = Compose (\x -> case isAFunctor s of Dict -> fmap (I x :*) (buildF s))

-- | Keep attempting to construct a data type until a constructor succeeds. The
-- first constructor to successfully be constructed (in the order defined in the
-- original data type) will be returned, or 'empty' if all constructions fail.
produceFirst
  :: (code ~ Code a, SingI code, Generic a, Alternative f)
  => NP (Producer f code) code -> f a
produceFirst = asum . produceM

-- | Produce all successful constructions of a data-type. If any constructors
-- fail, they will not be included in the resulting list. If all constructors
-- fail, this will return 'pure' '[]'.
produceAll
  :: (code ~ Code a, SingI code, Generic a, Alternative f)
  => NP (Producer f code) code -> f [a]
produceAll = fmap catMaybes . sequenceA . map optional . produceM

-- | Build a list of computations, one for each constructor in a data type.
produceM
  :: (code ~ Code a, SingI code, Generic a, Applicative f)
  => NP (Producer f code) code
  -> [f a]
produceM fs =
  map (fmap (to . SOP))
            (collapse_NP (fs `hap` injections))
