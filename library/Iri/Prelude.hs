module Iri.Prelude
(
  module Exports,
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

-- hashable
-------------------------
import Data.Hashable as Exports

-- vector-instances
-------------------------
import Data.Vector.Instances ()

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
