module Lazy
where

import Lazy.Prelude


newtype Lazy a = Lazy (IORef (Either (() -> a) a))

lazy :: (() -> a) -> Lazy a
lazy f =
  Lazy $ unsafePerformIO $ newIORef (Left f)

unlazy :: Lazy a -> a
unlazy (Lazy ref) =
  -- Synchronisation is not an issue here,
  -- since there's nothing dangerous in two threads
  -- occasionally computing the same result.
  -- That would be the price of not having 
  -- to pay for locks-maintenance overhead.
  unsafePerformIO $ readIORef ref >>= \case
    Left f -> do
      let a = f ()
      writeIORef ref (Right a)
      return a
    Right evaluated -> return evaluated
