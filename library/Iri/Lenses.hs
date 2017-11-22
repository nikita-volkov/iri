{-|
These are the beginnings of a Lens API.
It is compatible with the general Van Laarhoven lens libraries, such as \"lens\".

Many more definitions can be implemented, so do PRs if you miss any!
-}
module Iri.Lenses
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Rendering.ByteString as A
import qualified Iri.Parsing.ByteString as B
import qualified Iri.Rendering.Text as C
import qualified Iri.Parsing.Text as D


-- * Prisms
-------------------------

iriHttpIriPrism :: Prism' Iri HttpIri
iriHttpIriPrism =
  prism iriFromHttpIri (\ iri -> either (const (Left iri)) Right (httpIriFromIri iri))

uriByteStringIriPrism :: Prism' ByteString Iri
uriByteStringIriPrism =
  prism A.uri (\ bytes -> either (const (Left bytes)) Right (B.uri bytes))

uriByteStringHttpIriPrism :: Prism' ByteString HttpIri
uriByteStringHttpIriPrism =
  uriByteStringIriPrism . iriHttpIriPrism

iriTextIriPrism :: Prism' Text Iri
iriTextIriPrism =
  prism C.iri (\ text -> either (const (Left text)) Right (D.iri text))

iriTextHttpIriPrism :: Prism' Text HttpIri
iriTextHttpIriPrism =
  iriTextIriPrism . iriHttpIriPrism


-- * Lenses
-------------------------

iriSchemeLens :: Lens' Iri ByteString
iriSchemeLens =
  lens
    (\ (Iri (Scheme x) _ _ _) -> x)
    (\ (Iri _ hierarchy query fragment) x -> Iri (Scheme x) hierarchy query fragment)

iriHierarchyLens :: Lens' Iri Hierarchy
iriHierarchyLens =
  lens
    (\ (Iri _ x _ _) -> x)
    (\ (Iri scheme _ query fragment) x -> Iri scheme x query fragment)

iriQueryLens :: Lens' Iri Text
iriQueryLens =
  lens
    (\ (Iri _ _ (Query x) _) -> x)
    (\ (Iri scheme hierarchy _ fragment) x -> Iri scheme hierarchy (Query x) fragment)

iriFragmentLens :: Lens' Iri Text
iriFragmentLens =
  lens
    (\ (Iri _ _ _ (Fragment x)) -> x)
    (\ (Iri scheme hierarchy query _) x -> Iri scheme hierarchy query (Fragment x))
