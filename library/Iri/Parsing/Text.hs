module Iri.Parsing.Text
(
  iri,
  httpIri,
)
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Parsing.Attoparsec.Text as A
import qualified Data.Attoparsec.Text as B


{-|
Parser of a well-formed IRI conforming to the RFC3987 standard into 'Iri'.
Performs URL-decoding.
-}
iri :: Text -> Either Text Iri
iri =
  either (Left . fromString) Right .
  B.parseOnly (A.iri <* B.endOfInput)

{-|
Same as 'iri', but optimized specifially for the case of HTTP IRIs.
-}
httpIri :: Text -> Either Text HttpIri
httpIri =
  either (Left . fromString) Right .
  B.parseOnly (A.httpIri <* B.endOfInput)
