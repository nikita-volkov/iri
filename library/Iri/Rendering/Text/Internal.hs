module Iri.Rendering.Text.Internal where

import Iri.Data.Types
import Iri.Prelude
import Iri.Rendering.TextBuilder.Internal qualified as A
import Text.Builder qualified as B

-- | Render as a Unicode IRI text
iri :: Iri -> Text
iri =
  B.run . A.iri

-- | Render as a Unicode IRI text
httpIri :: HttpIri -> Text
httpIri =
  B.run . A.httpIri
