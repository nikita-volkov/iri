module Iri.Instances.Show
where

import Iri.Prelude
import Iri.Data.Internal
import qualified Iri.Rendering.Text.Internal as A
import qualified Data.Text as B


instance Show Iri where
  show =
    B.unpack . A.iri

instance Show HttpIri where
  show =
    B.unpack . A.httpIri
