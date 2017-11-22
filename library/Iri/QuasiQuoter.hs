module Iri.QuasiQuoter
(
  uri,
  httpUri,
)
where

import Iri.Prelude hiding (exp)
import Iri.Data
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Iri.Parsing.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as B
import qualified Data.Attoparsec.Text as C
import qualified Iri.Parsing.Attoparsec.Text as D


exp :: (String -> Q Exp) -> QuasiQuoter
exp exp =
  QuasiQuoter exp unsupported unsupported unsupported
  where
    unsupported _ =
      fail "Not supported"

{-|
IRI literal parsed from textual URI.
-}
uri :: QuasiQuoter
uri =
  exp $ \ string ->
  case B.parseOnly (A.uri <* B.endOfInput) (fromString string) of
    Right iri -> lift iri
    Left error -> fail (showString "URI parsing: " error)

{-|
HTTP IRI literal parsed from textual URI.
-}
httpUri :: QuasiQuoter
httpUri =
  exp $ \ string ->
  case B.parseOnly (A.httpUri <* B.endOfInput) (fromString string) of
    Right iri -> lift iri
    Left error -> fail (showString "HTTP URI parsing: " error)

{-|
IRI literal parsed from textual IRI.
-}
iri :: QuasiQuoter
iri =
  exp $ \ string ->
  case C.parseOnly (D.iri <* C.endOfInput) (fromString string) of
    Right iri -> lift iri
    Left error -> fail (showString "IRI parsing: " error)

{-|
HTTP IRI literal parsed from textual IRI.
-}
httpIri :: QuasiQuoter
httpIri =
  exp $ \ string ->
  case C.parseOnly (D.httpIri <* C.endOfInput) (fromString string) of
    Right iri -> lift iri
    Left error -> fail (showString "HTTP IRI parsing: " error)
