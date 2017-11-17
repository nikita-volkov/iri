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

scheme :: Scheme -> ByteString
scheme =
  F.poking . A.scheme

host :: Host -> ByteString
host =
  F.poking . A.host

pathAndQuery :: Path -> Query -> ByteString
pathAndQuery path query =
  F.poking (A.pathAndQuery path query)

path :: Path -> ByteString
path =
  F.poking . A.path

query :: Query -> ByteString
query =
  F.poking . A.query
