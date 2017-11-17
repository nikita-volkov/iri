module Iri.Parsing.ByteString
(
  url,
)
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Parsing.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as B


url :: ByteString -> Either Text Iri
url =
  either (Left . fromString) Right .
  B.parseOnly A.url
