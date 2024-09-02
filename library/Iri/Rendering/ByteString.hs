module Iri.Rendering.ByteString where

import Iri.Data
import Iri.Prelude
import Iri.Rendering.Ptr.Poking qualified as A
import Ptr.ByteString qualified as F

-- | Render as URI ASCII bytes
uri :: Iri -> ByteString
uri =
  F.poking . A.uri

-- | Render as URI ASCII bytes
httpUri :: HttpIri -> ByteString
httpUri =
  F.poking . A.httpUri
