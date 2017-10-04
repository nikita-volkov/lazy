module Lazy
(
  Lazy,
  lazy,
  unlazy,
)
where

import Lazy.Prelude


newtype Lazy value = Lazy (IORef (LazyState value))

data LazyState value = UnevaluatedLazyState value | EvaluatedLazyState !value

lazy :: value -> Lazy value
lazy value =
  Lazy (unsafeDupablePerformIO (newIORef (UnevaluatedLazyState value)))

unlazy :: Lazy value -> value
unlazy (Lazy ref) =
  -- Synchronisation is not an issue here,
  -- since there's nothing dangerous in two threads
  -- occasionally computing the same result.
  -- That would be the price of not having 
  -- to pay for locks-maintenance overhead.
  unsafeDupablePerformIO $ readIORef ref >>= \case
    EvaluatedLazyState !value -> return value
    UnevaluatedLazyState !value -> writeIORef ref (EvaluatedLazyState value) $> value
