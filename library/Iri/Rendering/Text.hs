module Iri.Rendering.Text
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Rendering.TextBuilder as A
import qualified Text.Builder as B


{-| Render as a Unicode IRI text -}
iri :: Iri -> Text
iri =
  B.run . A.iri
