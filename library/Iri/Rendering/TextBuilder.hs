module Iri.Rendering.TextBuilder
(
  iri,
)
where

import Iri.Prelude hiding (null)
import Iri.Data
import Text.Builder
import qualified Data.Text.Encoding as A
import qualified Data.Text.Encoding.Error as A
import qualified Data.Text.Punycode as B
import qualified Data.Text as C
import qualified Data.HashMap.Strict as G
import qualified Data.Vector as H
import qualified Net.IPv4 as D
import qualified Net.IPv6 as E
import qualified Iri.Vector as F


prependIfNotNull :: Builder -> Builder -> Builder
prependIfNotNull prepended it =
  if null it
    then mempty
    else prepended <> it

appendIfNotNull :: Builder -> Builder -> Builder
appendIfNotNull appended it =
  if null it
    then mempty
    else it <> appended

iri :: Iri -> Builder
iri (Iri schemeValue authorityValue hostValue portValue pathValue queryValue fragmentValue) =
  scheme schemeValue <> 
  text "://" <>
  (case authorityValue of
    PresentAuthority (User userValue) passwordValue ->
      text userValue <>
      (case passwordValue of
        PresentPassword passwordText -> char ':' <> text passwordText
        MissingPassword -> mempty) <>
      char '@'
    MissingAuthority -> mempty) <>
  host hostValue <>
  (prependIfNotNull
    (char '/')
    (path pathValue)) <>
  (prependIfNotNull
    (char '?')
    (query queryValue)) <>
  (prependIfNotNull
    (char '#')
    (fragment fragmentValue))

scheme :: Scheme -> Builder
scheme (Scheme bytes) =
  text (A.decodeUtf8With A.lenientDecode bytes)

host :: Host -> Builder
host =
  \ case
    NamedHost value -> idn value
    IpV4Host value -> ipV4 value
    IpV6Host value -> ipV6 value

idn :: Idn -> Builder
idn (Idn vector) =
  F.intercalate domainLabel (char '.') vector

domainLabel :: DomainLabel -> Builder
domainLabel (DomainLabel value) =
  text value

ipV4 :: IPv4 -> Builder
ipV4 =
  text . D.encode

ipV6 :: IPv6 -> Builder
ipV6 =
  text . E.encode

path :: Path -> Builder
path (Path pathSegmentVector) =
  F.intercalate pathSegment (char '/') pathSegmentVector

pathSegment :: PathSegment -> Builder
pathSegment (PathSegment value) =
  text value

query :: Query -> Builder
query (Query map) =
  F.intercalate
    (\ (key, value) -> text key <> prependIfNotNull (char '=') (text value))
    (char '&')
    map

fragment :: Fragment -> Builder
fragment (Fragment value) =
  text value
