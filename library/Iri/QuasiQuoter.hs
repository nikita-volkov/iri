module Iri.QuasiQuoter
where

import Iri.Prelude
import Iri.Data
import Iri.Instances.Lift
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Iri.Parsing.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as B


{-|
IRI literal parsed from URI.
-}
uri :: QuasiQuoter
uri =
  QuasiQuoter exp unsupported unsupported unsupported
  where
    unsupported _ =
      fail "Not supported"
    exp string =
      case B.parseOnly (A.uri <* B.endOfInput) (fromString string) of
        Right iri -> lift iri
        Left error -> fail (showString "URI parsing: " error)

{-|
IRI literal parsed from HTTP URI.
-}
httpUri :: QuasiQuoter
httpUri =
  QuasiQuoter exp unsupported unsupported unsupported
  where
    unsupported _ =
      fail "Not supported"
    exp string =
      case B.parseOnly (A.httpUri <* B.endOfInput) (fromString string) of
        Right iri -> lift iri
        Left error -> fail (showString "HTTP URI parsing: " error)
