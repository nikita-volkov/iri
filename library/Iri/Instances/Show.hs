module Iri.Instances.Show
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Rendering.Text as A
import qualified Data.Text as B


instance Show Iri where
  show =
    B.unpack . A.iri

instance Show HttpIri where
  show =
    B.unpack . A.httpIri
