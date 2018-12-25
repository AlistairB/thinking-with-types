{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module ChapterTwo.Solution where
{-
Exercise 2.1.3-i
If Show Int has kind CONSTRAINT what’s the kind of Show?

TYPE -> CONSTRAINT
-}

{-
Exercise 2.1.3-ii
What is the kind of Functor?

(TYPE -> TYPE) -> CONSTRAINT
-}

{-
Exercise 2.1.3-iii
What is the kind of Monad?

(TYPE -> TYPE) -> CONSTRAINT
-}

{-
Exercise 2.1.3-iv
What is the kind of MonadTrans?

I guess
(TYPE -> TYPE -> TYPE) -> CONSTRAINT

hmmm why
((TYPE → TYPE) → TYPE → TYPE) → CONSTRAINT

class MonadTrans t where
  lift :: Monad m => m a -> t m a

From `lift :: Monad m => m a -> t m a` can say `t` embeds a `m` which is a Monad which is (TYPE -> TYPE)

Therefore we start with (TYPE -> TYPE) -> ? . However, we also need an `a` type as well.
     Monad        A   produces a concrete type and we get a constraint
((TYPE → TYPE) → TYPE → TYPE) -> CONSTRAINT

`MaybeT (m :: * -> *) a` works as `(MonadTrans MaybeT) :: Constraint`
-}

{-
Exercise 2.4-i
Write a closed type family to compute Not.
-}

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or 'False y = y

type family Not (x :: Bool) :: Bool where
  Or 'True = 'False
  Or 'False = 'True
