{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module ChapterSeven.Solution where

import Data.Typeable (Typeable, cast)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import Data.Kind (Type, Constraint)

data Any = forall a. Any a

data Any2 where
  Any2 :: a -> Any2

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a


{-
Exercise 7.1-i
Are functions of type forall a. a -> r interesting?
Why or why not?

I think it's interesting because the function cannot care about what type you could be specialising to?

No because a must be isomorphic to r?

OR actual answer

These functions can only ever return constant values, as
the polymorphism on their input doesn’t allow any form
of inspection.

I think the common usage is side effecting functions which produce IO ()
-}

data HasShow where
  HasShow :: Show t => t -> HasShow

-- instance Show HasShow where
--   show (HasShow s) = "HasShow " ++ show s

{-
Exercise 7.1-ii
What happens to this instance if you remove the Show t => constraint from HasShow?

I think it can no longer use `show` and would need to be some hardcoded thing, which really violates the (read . show) == id law. You can't force t to require show as the `HasShow` type has no type variables.
-}

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

{-
Exercise 7.1-iii
Write the Show instance for HasShow in terms of
elimHasShow.
-}

instance Show HasShow where
  show = elimHasShow ((++) "HasShow " . show)

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

-- cast :: (Typeable a, Typeable b) => a -> Maybe b

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r.
  ( Typeable a
  , Typeable b
  , Typeable r
  )
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f =
  fmap Dynamic . f
    <$> fromDynamic d1
    <*> fromDynamic d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
  [ liftD2 @String @String a b (++)
  , liftD2 @Int    @Int    a b (+)
  , liftD2 @String @Int a b $ \strA intB ->
      strA ++ show intB
  , liftD2 @Int @String a b $ \intA strB ->
      show intA ++ strB
  ]

-- fromDynamic @Int ( pyPlus (Dynamic 1) (Dynamic 2))

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r
elimHas f (Has a) = f a

-- type HasShow = Has Show
-- type Dynamic = Has Typeable
