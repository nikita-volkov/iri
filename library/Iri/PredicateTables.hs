{-|
Predicate tables for implementing performant predicates.

Reference: <https://tools.ietf.org/html/rfc1738>.
-}
module Iri.PredicateTables
where

import Iri.Prelude
import qualified Data.Vector as A
import qualified Iri.CodePointPredicates as B


{-# NOINLINE scheme #-}
scheme :: Vector Bool
scheme =
  A.generate 256 B.scheme

{-# NOINLINE domainLabel #-}
domainLabel :: Vector Bool
domainLabel =
  A.generate 256 B.domainLabel

{-# NOINLINE unencodedPathSegment #-}
unencodedPathSegment :: Vector Bool
unencodedPathSegment =
  A.generate 256 B.unencodedPathSegment

{-# NOINLINE unencodedQueryComponent #-}
unencodedQueryComponent :: Vector Bool
unencodedQueryComponent =
  A.generate 256 B.unencodedQueryComponent

{-# INLINE unencodedFragment #-}
unencodedFragment :: Vector Bool
unencodedFragment =
  unencodedQueryComponent
