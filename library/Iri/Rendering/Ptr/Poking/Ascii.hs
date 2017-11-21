module Iri.Rendering.Ptr.Poking.Ascii
(
  uri,
)
where

import Iri.Prelude hiding (null, poke)
import Iri.Data
import Ptr.Poking
import qualified Data.Text.Encoding as A
import qualified Data.Text.Encoding.Error as A
import qualified Data.Text.Punycode as B
import qualified Data.Text as C
import qualified Data.HashMap.Strict as G
import qualified Data.Vector as H
import qualified Net.IPv4 as D
import qualified Net.IPv6 as E
import qualified Iri.Vector as F
import qualified Iri.CodePointPredicates.Core as I
import qualified Iri.CodePointPredicates.Rfc3986 as I
import qualified Iri.Utf8CodePoint as K
import qualified Iri.Rendering.Poke as L


uri :: Iri -> Poking
uri (Iri schemeValue hierarchyValue queryValue fragmentValue) =
  scheme schemeValue <> 
  asciiChar ':' <>
  hierarchy hierarchyValue <>
  (prependIfNotNull
    (asciiChar '?')
    (query queryValue)) <>
  (prependIfNotNull
    (asciiChar '#')
    (fragment fragmentValue))

scheme :: Scheme -> Poking
scheme (Scheme value) =
  bytes value

hierarchy :: Hierarchy -> Poking
hierarchy =
  \ case
    AuthorisedHierarchy authorityValue pathSegmentsValue ->
      bytes "//" <> authority authorityValue <> prependIfNotNull (asciiChar '/') (pathSegments pathSegmentsValue)
    AbsoluteHierarchy pathSegmentsValue ->
      asciiChar '/' <> pathSegments pathSegmentsValue
    RelativeHierarchy pathSegmentsValue ->
      pathSegments pathSegmentsValue

authority :: Authority -> Poking
authority (Authority userInfoValue hostValue portValue) =
  appendIfNotNull (asciiChar '@') (userInfo userInfoValue) <>
  host hostValue <>
  prependIfNotNull (asciiChar ':') (port portValue)

userInfo :: UserInfo -> Poking
userInfo =
  \ case
    PresentUserInfo (User user) password -> case password of
      PresentPassword password -> userInfoComponent user <> asciiChar ':' <> userInfoComponent password
      MissingPassword -> userInfoComponent user
    MissingUserInfo -> mempty

userInfoComponent :: Text -> Poking
userInfoComponent =
  urlEncodedText I.unencodedUserInfoComponent

host :: Host -> Poking
host =
  \ case
    NamedHost value -> domainName value
    IpV4Host value -> ipV4 value
    IpV6Host value -> ipV6 value

domainName :: RegName -> Poking
domainName (RegName vector) =
  F.intercalate domainLabel (asciiChar '.') vector

domainLabel :: DomainLabel -> Poking
domainLabel (DomainLabel value) =
  if C.all (< '\x80') value
    then bytes (A.encodeUtf8 value)
    else bytes "xn--" <> bytes (B.encode value)

ipV4 :: IPv4 -> Poking
ipV4 =
  bytes . A.encodeUtf8 . D.encode

ipV6 :: IPv6 -> Poking
ipV6 =
  bytes . A.encodeUtf8 . E.encode

port :: Port -> Poking
port =
  \ case
    PresentPort value -> asciiIntegral value
    MissingPort -> mempty

pathSegments :: PathSegments -> Poking
pathSegments (PathSegments pathSegmentVector) =
  F.intercalate pathSegment (asciiChar '/') pathSegmentVector

pathSegment :: PathSegment -> Poking
pathSegment (PathSegment value) =
  urlEncodedText I.unencodedPathSegment value

query :: Query -> Poking
query (Query value) =
  urlEncodedText I.unencodedQuery value

fragment :: Fragment -> Poking
fragment (Fragment value) =
  urlEncodedText I.unencodedFragment value

{-| Apply URL-encoding to text -}
urlEncodedText :: I.Predicate -> Text -> Poking
urlEncodedText unencodedPredicate =
  C.foldl' (\ poking -> mappend poking . urlEncodedUnicodeCodePoint unencodedPredicate . ord) mempty

urlEncodedUnicodeCodePoint :: I.Predicate -> Int -> Poking
urlEncodedUnicodeCodePoint unencodedPredicate codePoint =
  K.unicodeCodePoint codePoint
    (\ b1 -> if unencodedPredicate codePoint then word8 b1 else urlEncodedByte b1)
    (\ b1 b2 -> urlEncodedByte b1 <> urlEncodedByte b2)
    (\ b1 b2 b3 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3)
    (\ b1 b2 b3 b4 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3 <> urlEncodedByte b4)

urlEncodedByte :: Word8 -> Poking
urlEncodedByte =
  poke L.urlEncodedByte

prependIfNotNull :: Poking -> Poking -> Poking
prependIfNotNull prepended it =
  if null it
    then mempty
    else prepended <> it

appendIfNotNull :: Poking -> Poking -> Poking
appendIfNotNull appended it =
  if null it
    then mempty
    else it <> appended
