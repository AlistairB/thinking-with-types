{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module ChapterFour.Play where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy (..))

broken :: forall a b. (a -> b) -> a -> b
broken f a = apply
  where
    apply :: b
    apply = f a

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

type family AlwaysUnit a where
  AlwaysUnit a = ()

-- -- hmm how to I write this?
-- woah :: AlwaysUnit a
-- woah = AlwaysUnit @Bool
