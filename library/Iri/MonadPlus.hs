module Iri.MonadPlus
where

import Iri.Prelude hiding (null, length)


{-# INLINE foldl #-}
foldl :: MonadPlus m => (a -> b -> a) -> a -> m b -> m a
foldl step start elementParser =
  loop start
  where
    loop state =
      mplus
        (do
          element <- elementParser
          loop $! step state element)
        (return state)

{-# INLINE foldlM #-}
foldlM :: MonadPlus m => (a -> b -> m a) -> a -> m b -> m a
foldlM step start elementParser =
  loop start
  where
    loop state =
      mplus
        (do
          element <- elementParser
          loop =<< step state element)
        (return state)
