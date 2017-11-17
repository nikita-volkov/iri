module Iri.Rendering.Poking.Ascii
(
  uri,
  scheme,
  host,
  pathAndQuery,
  path,
  query,
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

{-| Apply URL-encoding to text -}
urlEncodedText :: I.Predicate -> Text -> Poking
urlEncodedText unencodedPredicate =
  C.foldl' (\ poking -> mappend poking . urlEncodedUnicodeCodePoint unencodedPredicate . ord) mempty

urlEncodedUnicodeCodePoint :: I.Predicate -> Int -> Poking
urlEncodedUnicodeCodePoint unencodedPredicate codePoint =
  if codePoint < 128 && unencodedPredicate codePoint
    then word8 (fromIntegral codePoint)
    else 
      K.unicodeCodePoint codePoint
        (\ b1 -> urlEncodedByte b1)
        (\ b1 b2 -> urlEncodedByte b1 <> urlEncodedByte b2)
        (\ b1 b2 b3 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3)
        (\ b1 b2 b3 b4 -> urlEncodedByte b1 <> urlEncodedByte b2 <> urlEncodedByte b3 <> urlEncodedByte b4)

urlEncodedByte :: Word8 -> Poking
urlEncodedByte =
  poke L.urlEncodedByte

uri :: Iri -> Poking
uri (Iri schemeValue authorityValue hostValue portValue pathValue queryValue fragmentValue) =
  scheme schemeValue <> 
  bytes "://" <>
  (case authorityValue of
    PresentAuthority (User userValue) passwordValue ->
      userInfoComponent userValue <>
      (case passwordValue of
        PresentPassword passwordValue -> asciiChar ':' <> userInfoComponent passwordValue
        MissingPassword -> mempty) <>
      asciiChar '@'
    MissingAuthority -> mempty) <>
  host hostValue <>
  prependIfNotNull
    (asciiChar '/')
    (mappend
      (mappend
        (path pathValue)
        (prependIfNotNull
          (asciiChar '?')
          (query queryValue)))
      (prependIfNotNull
        (asciiChar '#')
        (fragment fragmentValue)))

scheme :: Scheme -> Poking
scheme (Scheme value) =
  bytes value

userInfoComponent :: Text -> Poking
userInfoComponent =
  urlEncodedText I.unencodedUserInfoComponent

host :: Host -> Poking
host =
  \ case
    NamedHost value -> idn value
    IpV4Host value -> ipV4 value
    IpV6Host value -> ipV6 value

idn :: Idn -> Poking
idn (Idn vector) =
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

path :: Path -> Poking
path (Path pathSegmentVector) =
  F.intercalate pathSegment (asciiChar '/') pathSegmentVector

pathSegment :: PathSegment -> Poking
pathSegment (PathSegment value) =
  urlEncodedText I.unencodedPathSegment value

query :: Query -> Poking
query (Query value) =
  F.intercalate
    (\ (!key, !value) -> queryComponent key <> prependIfNotNull (asciiChar '=') (queryComponent value))
    (asciiChar '&')
    value

queryComponent :: Text -> Poking
queryComponent value =
  urlEncodedText I.unencodedQueryComponent value

fragment :: Fragment -> Poking
fragment (Fragment value) =
  urlEncodedText I.unencodedFragment value

{-|
Useful for HTTP requests.
-}
pathAndQuery :: Path -> Query -> Poking
pathAndQuery pathValue queryValue =
  asciiChar '/' <> path pathValue <> asciiChar '?' <> query queryValue
