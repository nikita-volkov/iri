module Iri.Rendering.Text.Internal
where

import Iri.Prelude
import Iri.Data.Internal
import qualified Iri.Rendering.TextBuilder.Internal as A
import qualified Text.Builder as B


{-| Render as a Unicode IRI text -}
iri :: Iri -> Text
iri =
  B.run . A.iri

{-| Render as a Unicode IRI text -}
httpIri :: HttpIri -> Text
httpIri =
  B.run . A.httpIri
