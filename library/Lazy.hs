module Lazy
(
  Lazy,
  lazy,
  unlazy,
)
where

import Lazy.Prelude hiding (lazy)


newtype Lazy value = Lazy (IO value)

instance Functor Lazy where
  {-# INLINE fmap #-}
  fmap mapping (Lazy io) =
    Lazy (fmap mapping io)

instance Applicative Lazy where
  pure = lazy
  {-# INLINE (<*>) #-}
  (<*>) (Lazy leftIO) (Lazy rightIO) =
    Lazy (leftIO <*> rightIO)

instance Monad Lazy where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (Lazy leftIO) rightK =
    Lazy $ do
      leftValue <- leftIO
      case rightK leftValue of
        Lazy rightIO -> rightIO

data LazyState value = UnevaluatedLazyState value | EvaluatedLazyState !value


{-# INLINE lazy #-}
lazy :: value -> Lazy value
lazy value =
  unsafeDupablePerformIO $ do
    stateRef <- newIORef (UnevaluatedLazyState value)
    return $ Lazy $ do
      state <- readIORef stateRef
      case state of
        EvaluatedLazyState !value -> return value
        UnevaluatedLazyState !value -> do
          writeIORef stateRef (EvaluatedLazyState value)
          return value

{-# INLINE unlazy #-}
unlazy :: Lazy value -> value
unlazy (Lazy io) =
  -- Synchronisation is not an issue here,
  -- since there's nothing dangerous in two threads
  -- occasionally computing the same result.
  -- That would be the price of not having 
  -- to pay for locks-maintenance overhead.
  unsafeDupablePerformIO io
