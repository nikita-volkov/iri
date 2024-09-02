module Iri.Rendering.TextBuilder.Internal
  ( iri,
    httpIri,
  )
where

import Data.ByteString qualified as ByteString
import Data.Text qualified as C
import Data.Text.Encoding qualified as A
import Data.Text.Encoding.Error qualified as A
import Iri.CodePointPredicates.Core qualified as CorePredicates
import Iri.CodePointPredicates.Rfc3986 qualified as Rfc3986Predicates
import Iri.CodePointPredicates.Rfc3987 qualified as Rfc3987Predicates
import Iri.Data.Types
import Iri.Prelude hiding (null)
import Iri.Utf8CodePoint qualified as K
import Iri.Vector qualified as F
import Net.IPv4 qualified as D
import Net.IPv6 qualified as E
import Text.Builder

iri :: Iri -> Builder
iri (Iri schemeValue hierarchyValue queryValue fragmentValue) =
  scheme schemeValue
    <> char ':'
    <> hierarchy hierarchyValue
    <> ( prependIfNotNull
           (char '?')
           (query queryValue)
       )
    <> ( prependIfNotNull
           (char '#')
           (fragment fragmentValue)
       )

httpIri :: HttpIri -> Builder
httpIri (HttpIri (Security secure) hostValue portValue pathValue queryValue fragmentValue) =
  (if secure then string "https://" else string "http://")
    <> host hostValue
    <> prependIfNotNull (char ':') (port portValue)
    <> prependIfNotNull (char '/') (path pathValue)
    <> prependIfNotNull (char '?') (query queryValue)
    <> prependIfNotNull (char '#') (fragment fragmentValue)

scheme :: Scheme -> Builder
scheme (Scheme bytes) =
  text (A.decodeUtf8With A.lenientDecode bytes)

hierarchy :: Hierarchy -> Builder
hierarchy =
  \case
    AuthorisedHierarchy authorityValue pathValue ->
      string "//" <> authority authorityValue <> prependIfNotNull (char '/') (path pathValue)
    AbsoluteHierarchy pathValue ->
      char '/' <> path pathValue
    RelativeHierarchy pathValue ->
      path pathValue

authority :: Authority -> Builder
authority (Authority userInfoValue hostValue portValue) =
  appendIfNotNull (char '@') (userInfo userInfoValue)
    <> host hostValue
    <> prependIfNotNull (char ':') (port portValue)

userInfo :: UserInfo -> Builder
userInfo =
  \case
    PresentUserInfo (User user) password -> case password of
      PresentPassword password -> userInfoComponent user <> char ':' <> userInfoComponent password
      MissingPassword -> userInfoComponent user
    MissingUserInfo -> mempty

userInfoComponent :: ByteString -> Builder
userInfoComponent =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedUserInfoComponent Rfc3986Predicates.unencodedUserInfoComponent

host :: Host -> Builder
host =
  \case
    NamedHost value -> domainName value
    IpV4Host value -> ipV4 value
    IpV6Host value -> ipV6 value

domainName :: RegName -> Builder
domainName (RegName vector) =
  F.intercalate domainLabel (char '.') vector

domainLabel :: DomainLabel -> Builder
domainLabel (DomainLabel x) =
  text x

ipV4 :: IPv4 -> Builder
ipV4 =
  text . D.encode

ipV6 :: IPv6 -> Builder
ipV6 =
  text . E.encode

port :: Port -> Builder
port =
  \case
    PresentPort value -> unsignedDecimal value
    MissingPort -> mempty

path :: Path -> Builder
path (Path pathSegmentVector) =
  F.intercalate pathSegment (char '/') pathSegmentVector

pathSegment :: PathSegment -> Builder
pathSegment (PathSegment value) =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedPathSegment Rfc3986Predicates.unencodedPathSegment value

query :: Query -> Builder
query (Query value) =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedQuery Rfc3986Predicates.unencodedQuery value

fragment :: Fragment -> Builder
fragment (Fragment value) =
  urlEncodedBytesOrText Rfc3987Predicates.unencodedFragment Rfc3986Predicates.unencodedFragment value

urlEncodedBytesOrText :: CorePredicates.Predicate -> CorePredicates.Predicate -> ByteString -> Builder
urlEncodedBytesOrText unencodedPredicate1 unencodedPredicate2 bytes =
  case A.decodeUtf8' bytes of
    Right text -> urlEncodedText unencodedPredicate1 text
    Left _ -> urlEncodedBytes unencodedPredicate2 bytes

-- | Apply URL-encoding to text
urlEncodedBytes :: CorePredicates.Predicate -> ByteString -> Builder
urlEncodedBytes unencodedPredicate =
  ByteString.foldl'
    ( \builder ->
        mappend builder . \byte ->
          if unencodedPredicate (fromIntegral byte)
            then utf8CodeUnits1 byte
            else urlEncodedByte byte
    )
    mempty

-- | Apply URL-encoding to text
urlEncodedText :: CorePredicates.Predicate -> Text -> Builder
urlEncodedText unencodedPredicate =
  C.foldl' (\builder -> mappend builder . urlEncodedUnicodeCodePoint unencodedPredicate . ord) mempty

urlEncodedUnicodeCodePoint :: CorePredicates.Predicate -> Int -> Builder
urlEncodedUnicodeCodePoint unencodedPredicate codePoint =
  if unencodedPredicate codePoint
    then unicodeCodePoint codePoint
    else
      K.unicodeCodePoint
        codePoint
        (\b1 -> urlEncodedByte b1)
        (\b1 b2 -> urlEncodedByte b1 <> urlEncodedByte b2)
        (\b1 b2 b3 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3)
        (\b1 b2 b3 b4 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3 <> urlEncodedByte b4)

urlEncodedByte :: Word8 -> Builder
urlEncodedByte x =
  case divMod x 16 of
    (d1, d2) -> char '%' <> hexadecimalDigit d1 <> hexadecimalDigit d2

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
