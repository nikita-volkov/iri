module Iri.QuasiQuoter
where

import Iri.Prelude
import Iri.Data
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Iri.Parsing.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as B


{-|
Parse URI.
-}
uri :: QuasiQuoter
uri =
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
      case B.parseOnly (A.uri <* B.endOfInput) (fromString string) of
        Right iri -> lift iri
        Left error -> fail (showString "URI parsing: " error)
      