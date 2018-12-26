module I where

{-
Consider the following type declarations. Which of them
have viable Functor instances?
-}

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap fab (T1 fa) = T1 $ fab . fa

newtype T2 a = T2 (a -> Int)

-- instance Functor T2 where
--   fmap fab (T2 faint) = T2 $ faint . fab
-- no way to compose the 2 functions

newtype T3 a = T3 (a -> a)

-- a -> b means the types can be different, but T3 enforces a -> a so not compatible

newtype T4 a = T4 ((Int -> a) -> Int)

-- no composition points
-- instance Functor T4 where
--   fmap fab (T4 iai) = T4 $ \ib -> iai $ ib . fab

newtype T5 a = T5 ((a -> Int) -> Int)

-- hehe couldn't figure this out. Makes sense now though

instance Functor T5 where
  fmap fab (T5 aii) = T5 $ \bi -> aii $ bi . fab
