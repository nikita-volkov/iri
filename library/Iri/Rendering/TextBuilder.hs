module Iri.Rendering.TextBuilder
(
  iri,
  httpIri,
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
iri (Iri schemeValue hierarchyValue queryValue fragmentValue) =
  scheme schemeValue <> 
  char ':' <>
  hierarchy hierarchyValue <>
  (prependIfNotNull
    (char '?')
    (query queryValue)) <>
  (prependIfNotNull
    (char '#')
    (fragment fragmentValue))

httpIri :: HttpIri -> Builder
httpIri (HttpIri (Security secure) hostValue portValue pathSegmentsValue queryValue fragmentValue) =
  (if secure then string "https://" else string "http://") <>
  host hostValue <>
  prependIfNotNull (char ':') (port portValue) <>
  prependIfNotNull (char '/') (pathSegments pathSegmentsValue) <>
  prependIfNotNull (char '?') (query queryValue) <>
  prependIfNotNull (char '#') (fragment fragmentValue)

scheme :: Scheme -> Builder
scheme (Scheme bytes) =
  text (A.decodeUtf8With A.lenientDecode bytes)

hierarchy :: Hierarchy -> Builder
hierarchy =
  \ case
    AuthorisedHierarchy authorityValue pathSegmentsValue ->
      string "//" <> authority authorityValue <> prependIfNotNull (char '/') (pathSegments pathSegmentsValue)
    AbsoluteHierarchy pathSegmentsValue ->
      char '/' <> pathSegments pathSegmentsValue
    RelativeHierarchy pathSegmentsValue ->
      pathSegments pathSegmentsValue

authority :: Authority -> Builder
authority (Authority userInfoValue hostValue portValue) =
  appendIfNotNull (char '@') (userInfo userInfoValue) <>
  host hostValue <>
  prependIfNotNull (char ':') (port portValue)

userInfo :: UserInfo -> Builder
userInfo =
  \ case
    PresentUserInfo (User user) password -> case password of
      PresentPassword password -> text user <> char ':'  <> text password
      MissingPassword -> text user
    MissingUserInfo -> mempty

host :: Host -> Builder
host =
  \ case
    NamedHost value -> domainName value
    IpV4Host value -> ipV4 value
    IpV6Host value -> ipV6 value

domainName :: RegName -> Builder
domainName (RegName vector) =
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

port :: Port -> Builder
port =
  \ case
    PresentPort value -> integral value
    MissingPort -> mempty

pathSegments :: PathSegments -> Builder
pathSegments (PathSegments pathSegmentVector) =
  F.intercalate pathSegment (char '/') pathSegmentVector

pathSegment :: PathSegment -> Builder
pathSegment (PathSegment value) =
  text value

query :: Query -> Builder
query (Query value) =
  text value

fragment :: Fragment -> Builder
fragment (Fragment value) =
  text value
