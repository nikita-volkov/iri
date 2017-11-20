module Iri.Rendering.ByteString.Ascii
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Rendering.Ptr.Poking.Ascii as A
import qualified Ptr.ByteString as F


{-| Render as URI ASCII bytes -}
uri :: Iri -> ByteString
uri =
  F.poking . A.uri
