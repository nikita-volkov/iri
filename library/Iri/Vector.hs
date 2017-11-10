module Iri.Vector
where

import Iri.Prelude hiding (null, length)
import Data.Vector


{-# INLINE intercalate #-}
intercalate :: (Semigroup monoid, Monoid monoid) => (element -> monoid) -> monoid -> Vector element -> monoid
intercalate project separator vector =
  if null vector
    then mempty
    else project (unsafeIndex vector 0) <> iterate 1
  where
    vectorLength =
      length vector
    iterate !index =
      if index < vectorLength
        then separator <> project (unsafeIndex vector index) <> iterate (succ index)
        else mempty
