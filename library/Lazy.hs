module Lazy
(
  Lazy,
  lazy,
  unlazy,
)
where

import Lazy.Prelude hiding (lazy)


data Lazy value = forall x. Lazy !(IORef (LazyState x)) !(x -> value)

instance Functor Lazy where
  {-# INLINE fmap #-}
  fmap mapping (Lazy ref proj) =
    Lazy ref (mapping . proj)

data LazyState value = UnevaluatedLazyState value | EvaluatedLazyState !value

{-# INLINE lazy #-}
lazy :: value -> Lazy value
lazy value =
  Lazy (unsafeDupablePerformIO (newIORef (UnevaluatedLazyState value))) id

unlazy :: Lazy value -> value
unlazy (Lazy ref proj) =
  -- Synchronisation is not an issue here,
  -- since there's nothing dangerous in two threads
  -- occasionally computing the same result.
  -- That would be the price of not having 
  -- to pay for locks-maintenance overhead.
  unsafeDupablePerformIO $ readIORef ref >>= \case
    EvaluatedLazyState !value -> return (proj value)
    UnevaluatedLazyState !value -> writeIORef ref (EvaluatedLazyState value) $> proj value
