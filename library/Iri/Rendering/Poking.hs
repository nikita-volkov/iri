module Iri.Rendering.Poking
where

import Iri.Prelude
import Iri.Data
import Ptr.Poking
import qualified Data.Text as C


domainLabel :: DomainLabel -> Poking
domainLabel (DomainLabel value) =
  if C.all (< '\x80') value
    then $(todo "")
    else bytes "xn--" <> $(todo "")
