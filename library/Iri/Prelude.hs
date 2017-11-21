module Iri.Prelude
(
  module Exports,
  Prism,
  Prism',
  Lens,
  Lens',
  prism,
  lens,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, (<>), First(..), Last(..), ProtocolError, traceEvent, traceEventIO, traceMarker, traceMarkerIO)

-- base
-------------------------
import Foreign as Exports hiding (void)

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- profunctors
-------------------------
import Data.Profunctor.Unsafe as Exports
import Data.Profunctor.Choice as Exports
import Data.Profunctor.Strong as Exports

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

-- th-lift-instances
-------------------------
import Instances.TH.Lift as Exports

-- bug
-------------------------
import Bug as Exports

--------------------------------------------------------------------------------

import qualified Language.Haskell.TH.Lift as A
A.deriveLift ''IPv4
A.deriveLift ''IPv6

--------------------------------------------------------------------------------

type Simple f s a = f s s a a

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

{-# INLINE prism #-}
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta =
  dimap seta (either pure (fmap bt)) . right'

{-# INLINE lens #-}
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s =
  sbt s <$> afb (sa s)
