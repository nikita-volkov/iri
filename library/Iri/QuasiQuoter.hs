module Iri.QuasiQuoter
where

import Iri.Prelude
import Iri.Data
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Iri.Parsing.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as B


{-|
Parse URL.
-}
url :: QuasiQuoter
url =
  QuasiQuoter {
    quoteExp = exp,
    quotePat = unsupported,
    quoteType = unsupported,
    quoteDec = unsupported
  }
  where
    unsupported _ =
      fail "Not supported"
    exp string =
      case B.parseOnly (A.url <* B.endOfInput) (fromString string) of
        Right iri -> lift iri
        Left error -> fail (showString "IRI parsing: " error)
      