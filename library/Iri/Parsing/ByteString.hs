module Iri.Parsing.ByteString
(
  uri,
  httpUri,
)
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Parsing.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as B


{-|
Parser of a well-formed URI conforming to the RFC3986 standard into IRI.
Performs URL- and Punycode-decoding.
-}
uri :: ByteString -> Either Text Iri
uri =
  either (Left . fromString) Right .
  B.parseOnly (A.uri <* B.endOfInput)

{-|
Same as 'uri', but optimized specifially for the case of HTTP URIs.
-}
httpUri :: ByteString -> Either Text HttpIri
httpUri =
  either (Left . fromString) Right .
  B.parseOnly (A.httpUri <* B.endOfInput)
