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

From `lift :: Monad m => m a -> t m a` can say `t` must have a Monad inside it. The kind of Monad is (TYPE -> TYPE) -> CONSTRAINT

Therefore we start with (TYPE -> TYPE) -> ? . However, in this case we need a TYPE not a CONSTRAINT, therefore we take the `a` type as well.
     Monad        A   produces a concrete type and we get a constraint
((TYPE → TYPE) → TYPE → TYPE) -> CONSTRAINT

`MaybeT (m :: * -> *) a` works as `(MonadTrans MaybeT) :: Constraint`
-}
