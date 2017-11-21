{-|
These are the beginnings of a Lens API.
It is compatible with the general Van Laarhoven lens libraries, such as \"lens\".

Many more definitions can be implemented, so do PRs if you miss any!
-}
module Iri.Lenses
where

import Iri.Prelude
import Iri.Data


iriHttpIriPrism :: Prism' Iri HttpIri
iriHttpIriPrism =
  prism iriFromHttpIri (\ iri -> either (const (Left iri)) Right (httpIriFromIri iri))

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
