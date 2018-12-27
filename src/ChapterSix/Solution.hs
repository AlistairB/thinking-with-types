{-# LANGUAGE RankNTypes #-}
module ChapterSix.Solution where

import Control.Monad (forever)

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

{-
Exercise 6.3-i
What is the rank of Int -> forall a. a -> a? Hint: try
adding the explicit parentheses.

1
-}

{-

Exercise 6.3-ii
What is the rank of (a -> b) -> (forall c. c -> a) -> b?

Hint: recall that the function arrow is right-associative, so a -> b -> c is actually parsed as a -> (b -> c).

2
-}

{-
Exercise 6.3-iii
What is the rank of ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a?

Believe it or not, this is a real type signature we had to write back in the bad old days before MonadUnliftIO!

3
-}

cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f =
  let callback = id
  in f callback


onInput :: (String -> IO ()) -> IO ()
onInput f = forever $ do
  str <- getLine
  f str

{-
Exercise 6.4-i
Provide a Functor instance for Cont. Hint: use lots of
type holes, and an explicit lambda whenever
looking for a function type. The implementation
is sufficiently difficult that trying to write it
point-free will be particularly mind-bending.
-}

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

instance Functor Cont where
  fmap fab (Cont farr)  = Cont $ \fbr -> farr (fbr . fab)

instance Applicative Cont where
  pure a = Cont $ \far -> far a
  (<*>) (Cont fabrr) (Cont farr)  = Cont $ \fbr ->
                                      fabrr $ \fab -> farr (fbr . fab)

instance Monad Cont where
  (>>=) (Cont mfarr) fCont = Cont $ \cfarr ->
                             mfarr $ \a -> unCont (fCont a) cfarr

newtype ContT m a = ContT
  { unContT :: forall r. (a -> m r) -> m r
  }
