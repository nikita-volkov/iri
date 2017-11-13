module Iri.CodePointPredicates.Core
where

import Iri.Prelude hiding ((|||), (&&&), inRange)
import qualified Data.Vector as A


type Predicate =
  Int -> Bool

{-# NOINLINE cached #-}
cached :: Int -> Predicate -> Predicate
cached size predicate =
  case A.generate 256 predicate of
    vector -> A.unsafeIndex vector

oneOfChars :: [Char] -> Predicate
oneOfChars string i =
  elem i (fmap ord string)

(|||) :: Predicate -> Predicate -> Predicate
(|||) left right i =
  left i || right i

(&&&) :: Predicate -> Predicate -> Predicate
(&&&) left right i =
  left i && right i

inRange :: Int -> Int -> Predicate
inRange min max i =
  i >= min && i <= max

inCharRange :: Char -> Char -> Predicate
inCharRange min max =
  inRange (ord min) (ord max)
