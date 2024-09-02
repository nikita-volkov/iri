{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Iri.Rendering.Ptr.Poking
  ( uri,
    httpUri,
    scheme,
    host,
    path,
    query,
  )
where

import Data.ByteString qualified as ByteString
import Data.Text qualified as C
import Data.Text.Encoding qualified as A
import Data.Text.Punycode qualified as B
import Iri.CodePointPredicates.Core qualified as I
import Iri.CodePointPredicates.Rfc3986 qualified as I
import Iri.Data
import Iri.Prelude hiding (null, poke)
import Iri.Rendering.Ptr.Poke qualified as L
import Iri.Utf8CodePoint qualified as K
import Iri.Vector qualified as F
import Net.IPv4 qualified as D
import Net.IPv6 qualified as E
import Ptr.Poking

uri :: Iri -> Poking
uri (Iri schemeValue hierarchyValue queryValue fragmentValue) =
  scheme schemeValue
    <> asciiChar ':'
    <> hierarchy hierarchyValue
    <> ( prependIfNotNull
           (asciiChar '?')
           (query queryValue)
       )
    <> ( prependIfNotNull
           (asciiChar '#')
           (fragment fragmentValue)
       )

httpUri :: HttpIri -> Poking
httpUri (HttpIri (Security secure) hostValue portValue pathValue queryValue fragmentValue) =
  (if secure then "https://" else "http://")
    <> host hostValue
    <> prependIfNotNull (asciiChar ':') (port portValue)
    <> prependIfNotNull (asciiChar '/') (path pathValue)
    <> prependIfNotNull (asciiChar '?') (query queryValue)
    <> prependIfNotNull (asciiChar '#') (fragment fragmentValue)

scheme :: Scheme -> Poking
scheme (Scheme value) =
  bytes value

hierarchy :: Hierarchy -> Poking
hierarchy =
  \case
    AuthorisedHierarchy authorityValue pathValue ->
      bytes "//" <> authority authorityValue <> prependIfNotNull (asciiChar '/') (path pathValue)
    AbsoluteHierarchy pathValue ->
      asciiChar '/' <> path pathValue
    RelativeHierarchy pathValue ->
      path pathValue

authority :: Authority -> Poking
authority (Authority userInfoValue hostValue portValue) =
  appendIfNotNull (asciiChar '@') (userInfo userInfoValue)
    <> host hostValue
    <> prependIfNotNull (asciiChar ':') (port portValue)

userInfo :: UserInfo -> Poking
userInfo =
  \case
    PresentUserInfo (User user) password -> case password of
      PresentPassword password -> userInfoComponent user <> asciiChar ':' <> userInfoComponent password
      MissingPassword -> userInfoComponent user
    MissingUserInfo -> mempty

userInfoComponent :: ByteString -> Poking
userInfoComponent =
  urlEncodedBytes I.unencodedUserInfoComponent

host :: Host -> Poking
host =
  \case
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
  \case
    PresentPort value -> asciiIntegral value
    MissingPort -> mempty

path :: Path -> Poking
path (Path pathSegmentVector) =
  F.intercalate pathSegment (asciiChar '/') pathSegmentVector

pathSegment :: PathSegment -> Poking
pathSegment (PathSegment value) =
  urlEncodedBytes I.unencodedPathSegment value

query :: Query -> Poking
query (Query value) =
  urlEncodedBytes I.unencodedQuery value

fragment :: Fragment -> Poking
fragment (Fragment value) =
  urlEncodedBytes I.unencodedFragment value

urlEncodedBytes :: I.Predicate -> ByteString -> Poking
urlEncodedBytes unencodedPredicate =
  ByteString.foldl' (\poking -> mappend poking . byte) mempty
  where
    byte x =
      if unencodedPredicate (fromIntegral x)
        then word8 x
        else urlEncodedByte x

-- | Apply URL-encoding to text
urlEncodedText :: I.Predicate -> Text -> Poking
urlEncodedText unencodedPredicate =
  C.foldl' (\poking -> mappend poking . urlEncodedUnicodeCodePoint unencodedPredicate . ord) mempty

urlEncodedUnicodeCodePoint :: I.Predicate -> Int -> Poking
urlEncodedUnicodeCodePoint unencodedPredicate codePoint =
  K.unicodeCodePoint
    codePoint
    (\b1 -> if unencodedPredicate codePoint then word8 b1 else urlEncodedByte b1)
    (\b1 b2 -> urlEncodedByte b1 <> urlEncodedByte b2)
    (\b1 b2 b3 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3)
    (\b1 b2 b3 b4 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3 <> urlEncodedByte b4)

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
