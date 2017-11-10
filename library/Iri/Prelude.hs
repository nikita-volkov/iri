module Iri.Prelude
(
  module Exports,
  foldlMonadPlus,
  foldlMMonadPlus,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, (<>), First(..), Last(..), ProtocolError, traceEvent, traceEventIO, traceMarker, traceMarkerIO)

-- base
-------------------------
import Foreign as Exports hiding (void)

-- semigroups
-------------------------
import Data.Semigroup as Exports

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- text
-------------------------
import Data.Text as Exports (Text)

-- ip
-------------------------
import Net.IPv4 as Exports (IPv4)
import Net.IPv6 as Exports (IPv6)

-- vector
-------------------------
import Data.Vector as Exports (Vector)

-- unordered-containers
-------------------------
import Data.HashMap.Strict as Exports (HashMap)

-- bug
-------------------------
import Bug as Exports

--------------------------------------------------------------------------------

{-# INLINE foldlMonadPlus #-}
foldlMonadPlus :: MonadPlus m => (a -> b -> a) -> a -> m b -> m a
foldlMonadPlus step start elementParser =
  loop start
  where
    loop state =
      mplus
        (do
          element <- elementParser
          loop $! step state element)
        (return state)

{-# INLINE foldlMMonadPlus #-}
foldlMMonadPlus :: MonadPlus m => (a -> b -> m a) -> a -> m b -> m a
foldlMMonadPlus step start elementParser =
  loop start
  where
    loop state =
      mplus
        (do
          element <- elementParser
          loop =<< step state element)
        (return state)
