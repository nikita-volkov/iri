{-|
These are the beginnings of a Lens API.
It is compatible with the general Van Laarhoven lens libraries, such as \"lens\".

Many more definitions can be implemented, so do PRs if you miss any!
-}
module Iri.Optics
(
  -- * Definitions
  Lens,
  Lens',
  Prism,
  Prism',
  -- * Prisms
  iriHttpIri,
  uriByteStringIri,
  uriByteStringHttpIri,
  iriTextIri,
  iriTextHttpIri,
  -- * Lenses
  iriScheme,
  iriHierarchy,
  iriQuery,
  iriFragment,
)
where

import Iri.Prelude
import Iri.Data
import qualified Iri.Rendering.ByteString as A
import qualified Iri.Parsing.ByteString as B
import qualified Iri.Rendering.Text as C
import qualified Iri.Parsing.Text as D


type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

{-# INLINE prism #-}
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta =
  dimap seta (either pure (fmap bt)) . right'

{-# INLINE lens #-}
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s =
  sbt s <$> afb (sa s)

-- * Prisms
-------------------------

iriHttpIri :: Prism' Iri HttpIri
iriHttpIri =
  prism iriFromHttpIri (\ iri -> either (const (Left iri)) Right (httpIriFromIri iri))

uriByteStringIri :: Prism' ByteString Iri
uriByteStringIri =
  prism A.uri (\ bytes -> either (const (Left bytes)) Right (B.uri bytes))

uriByteStringHttpIri :: Prism' ByteString HttpIri
uriByteStringHttpIri =
  uriByteStringIri . iriHttpIri

iriTextIri :: Prism' Text Iri
iriTextIri =
  prism C.iri (\ text -> either (const (Left text)) Right (D.iri text))

iriTextHttpIri :: Prism' Text HttpIri
iriTextHttpIri =
  iriTextIri . iriHttpIri


-- * Lenses
-------------------------

iriScheme :: Lens' Iri ByteString
iriScheme =
  lens
    (\ (Iri (Scheme x) _ _ _) -> x)
    (\ (Iri _ hierarchy query fragment) x -> Iri (Scheme x) hierarchy query fragment)

iriHierarchy :: Lens' Iri Hierarchy
iriHierarchy =
  lens
    (\ (Iri _ x _ _) -> x)
    (\ (Iri scheme _ query fragment) x -> Iri scheme x query fragment)

iriQuery :: Lens' Iri Text
iriQuery =
  lens
    (\ (Iri _ _ (Query x) _) -> x)
    (\ (Iri scheme hierarchy _ fragment) x -> Iri scheme hierarchy (Query x) fragment)

iriFragment :: Lens' Iri Text
iriFragment =
  lens
    (\ (Iri _ _ _ (Fragment x)) -> x)
    (\ (Iri scheme hierarchy query _) x -> Iri scheme hierarchy query (Fragment x))
