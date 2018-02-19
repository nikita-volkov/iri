module Iri.Parsing.ByteString
(
  uri,
  httpUri,
  regName,
)
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Parsing.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as B


parser parser =
  either (Left . fromString) Right .
  B.parseOnly (parser <* B.endOfInput)

{-|
Parser of a well-formed URI conforming to the RFC3986 standard into IRI.
Performs URL- and Punycode-decoding.
-}
uri :: ByteString -> Either Text Iri
uri =
  parser A.uri

{-|
Same as 'uri', but optimized specifially for the case of HTTP URIs.
-}
httpUri :: ByteString -> Either Text HttpIri
httpUri =
  parser A.httpUri

{-|
Domain name parser.
-}
regName :: ByteString -> Either Text RegName
regName =
  parser A.regName
