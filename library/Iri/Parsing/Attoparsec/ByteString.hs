module Iri.Parsing.Attoparsec.ByteString
(
  uri,
  httpUri,
)
where

import Iri.Prelude hiding (foldl, hash)
import Iri.Data
import Data.Attoparsec.ByteString hiding (try)
import qualified Data.Attoparsec.ByteString.Char8 as F
import qualified Data.ByteString as K
import qualified Data.Text as T
import qualified Data.Text.Punycode as A
import qualified Data.Text.Encoding as B
import qualified Data.Text.Encoding.Error as L
import qualified Data.HashMap.Strict as O
import qualified Data.Vector as S
import qualified VectorBuilder.Builder as P
import qualified VectorBuilder.Vector as Q
import qualified VectorBuilder.MonadPlus as E
import qualified Iri.PercentEncoding as I
import qualified Iri.CodePointPredicates.Rfc3986 as C
import qualified Iri.MonadPlus as R
import qualified Ptr.Poking as G
import qualified Ptr.ByteString as H
import qualified Text.Builder as J
import qualified Net.IPv4 as M
import qualified Net.IPv6 as N


{-# INLINE percent #-}
percent :: Parser Word8
percent =
  word8 37

{-# INLINE colon #-}
colon :: Parser Word8
colon =
  word8 58

{-# INLINE at #-}
at :: Parser Word8
at =
  word8 64

{-# INLINE forwardSlash #-}
forwardSlash :: Parser Word8
forwardSlash =
  word8 47

{-# INLINE question #-}
question :: Parser Word8
question =
  word8 63

{-# INLINE hash #-}
hash :: Parser Word8
hash =
  word8 35

{-# INLINE equality #-}
equality :: Parser Word8
equality =
  word8 61

{-# INLINE ampersand #-}
ampersand :: Parser Word8
ampersand =
  word8 38

{-# INLINE semicolon #-}
semicolon :: Parser Word8
semicolon =
  word8 59

{-# INLINE labeled #-}
labeled :: String -> Parser a -> Parser a
labeled label parser =
  parser <?> label

{-|
Parser of a well-formed URI conforming to the RFC3986 standard into IRI.
Performs URL- and Punycode-decoding.
-}
{-# INLINABLE uri #-}
uri :: Parser Iri
uri =
  labeled "URI" $ do
    parsedScheme <- scheme
    colon
    parsedHierarchy <- hierarchy
    parsedQuery <- query
    parsedFragment <- fragment
    return (Iri parsedScheme parsedHierarchy parsedQuery parsedFragment)

{-|
Same as 'uri', but optimized specifially for the case of HTTP URIs.
-}
{-# INLINABLE httpUri #-}
httpUri :: Parser HttpIri
httpUri =
  labeled "HTTP URI" $ do
    satisfy (\ x -> x == 104 || x == 72)
    satisfy (\ x -> x == 116 || x == 84)
    satisfy (\ x -> x == 116 || x == 84)
    satisfy (\ x -> x == 112 || x == 80)
    secure <- satisfy (\ b -> b == 115 || b == 83) $> True <|> pure False
    string "://"
    parsedHost <- host
    parsedPort <- PresentPort <$> (colon *> port) <|> pure MissingPort
    parsedPathSegments <- (forwardSlash *> pathSegments) <|> pure (PathSegments mempty)
    parsedQuery <- query
    parsedFragment <- fragment
    return (HttpIri (Security secure) parsedHost parsedPort parsedPathSegments parsedQuery parsedFragment)

{-# INLINE hierarchy #-}
hierarchy :: Parser Hierarchy
hierarchy =
  do
    slashPresent <- forwardSlash $> True <|> pure False
    if slashPresent
      then do
        slashPresent <- forwardSlash $> True <|> pure False
        if slashPresent
          then authorisedHierarchyBody AuthorisedHierarchy
          else AbsoluteHierarchy <$> pathSegments
      else RelativeHierarchy <$> pathSegments

{-# INLINE authorisedHierarchyBody #-}
authorisedHierarchyBody :: (Authority -> PathSegments -> body) -> Parser body
authorisedHierarchyBody body =
  do
    parsedUserInfo <- (presentUserInfo PresentUserInfo <* at) <|> pure MissingUserInfo
    parsedHost <- host
    parsedPort <- PresentPort <$> (colon *> port) <|> pure MissingPort
    parsedPathSegments <- (forwardSlash *> pathSegments) <|> pure (PathSegments mempty)
    return (body (Authority parsedUserInfo parsedHost parsedPort) parsedPathSegments)

{-# INLINE scheme #-}
scheme :: Parser Scheme
scheme =
  labeled "Scheme" $
  fmap Scheme (takeWhile1 (C.scheme . fromIntegral))

{-# INLINABLE presentUserInfo #-}
presentUserInfo :: (User -> Password -> a) -> Parser a
presentUserInfo result =
  labeled "User info" $
  do
    user <- User <$> urlEncodedString (C.unencodedPathSegment . fromIntegral)
    passwordFollows <- True <$ colon <|> pure False
    if passwordFollows
      then do
        password <- PresentPassword <$> urlEncodedString (C.unencodedPathSegment . fromIntegral)
        return (result user password)
      else return (result user MissingPassword)

{-# INLINE host #-}
host :: Parser Host
host =
  labeled "Host" $
  IpV6Host <$> ipV6 <|>
  IpV4Host <$> M.parserUtf8 <|>
  NamedHost <$> domainName

{-# INLINABLE ipV6 #-}
ipV6 :: Parser IPv6
ipV6 =
  do
    a <- F.hexadecimal
    colon
    b <- F.hexadecimal
    colon
    c <- F.hexadecimal
    colon
    d <- F.hexadecimal
    colon
    mplus
      (do
        e <- F.hexadecimal
        colon
        f <- F.hexadecimal
        colon
        g <- F.hexadecimal
        colon
        h <- F.hexadecimal
        return (N.fromWord16s a b c d e f g h))
      (do
        colon
        return (N.fromWord16s a b c d 0 0 0 0))

{-# INLINE domainName #-}
domainName :: Parser RegName
domainName =
  fmap RegName (E.sepBy1 domainLabel (word8 46))

{-|
Domain label with Punycode decoding applied.
-}
{-# INLINE domainLabel #-}
domainLabel :: Parser DomainLabel
domainLabel =
  labeled "Domain label" $ do
    punycodeFollows <- True <$ string "xn--" <|> pure False
    ascii <- takeWhile1 (C.domainLabel . fromIntegral)
    if punycodeFollows
      then case A.decode ascii of
        Right text -> return (DomainLabel text)
        Left exception -> fail (showString "Punycode decoding exception: " (show exception))
      else return (DomainLabel (B.decodeUtf8 ascii))

{-# INLINE port #-}
port :: Parser Word16
port =
  F.decimal

{-# INLINE path #-}
path :: Parser Path
path =
  (forwardSlash *> (AbsolutePath <$> pathSegments)) <|>
  (RelativePath <$> pathSegments)

{-# INLINE pathSegments #-}
pathSegments :: Parser PathSegments
pathSegments =
  do
    segments <- E.sepBy pathSegment forwardSlash
    if segmentsAreEmpty segments
      then return (PathSegments mempty)
      else return (PathSegments segments)
  where
    segmentsAreEmpty segments =
      S.length segments == 1 &&
      (case S.unsafeHead segments of PathSegment headSegmentText -> T.null headSegmentText)

{-# INLINE pathSegment #-}
pathSegment :: Parser PathSegment
pathSegment =
  fmap PathSegment (urlEncodedString (C.unencodedPathSegment . fromIntegral))

{-# INLINABLE urlEncodedString #-}
urlEncodedString :: (Word8 -> Bool) -> Parser Text
urlEncodedString unencodedBytesPredicate =
  labeled "URL-encoded string" $
  R.foldlM progress (mempty, mempty, B.streamDecodeUtf8) partPoking >>= finish
  where
    progress (!builder, _, decode) bytes =
      case unsafeDupablePerformIO (try (evaluate (decode bytes))) of
        Right (B.Some decodedChunk undecodedBytes newDecode) ->
          return (builder <> J.text decodedChunk, undecodedBytes, newDecode)
        Left (L.DecodeError error _) ->
          fail (showString "UTF8 decoding: " error)
    finish (builder, undecodedBytes, _) =
      if K.null undecodedBytes
        then return (J.run builder)
        else fail (showString "UTF8 decoding: Bytes remaining: " (show undecodedBytes))
    partPoking =
      takeWhile1 unencodedBytesPredicate <|> encoded
      where
        encoded =
          K.singleton <$> percentEncodedByte

{-# INLINE percentEncodedByte #-}
percentEncodedByte :: Parser Word8
percentEncodedByte =
  do
    percent 
    byte1 <- anyWord8
    byte2 <- anyWord8
    I.matchPercentEncodedBytes (fail "Broken percent encoding") return byte1 byte2

{-# INLINABLE query #-}
query :: Parser Query
query =
  labeled "Query" $
  (question *> queryBody) <|> pure (Query mempty)

{-|
The stuff after the question mark.
-}
{-# INLINABLE queryBody #-}
queryBody :: Parser Query
queryBody =
  fmap Query (urlEncodedString (C.unencodedQuery . fromIntegral))

{-# INLINABLE queryPair #-}
queryPair :: (Text -> Text -> a) -> Parser a
queryPair result =
  do
    !key <- urlEncodedString (C.unencodedQueryComponent . fromIntegral)
    when (T.null key) (fail "Key is empty")
    optional (string "[]")
    !value <- (equality *> urlEncodedString (C.unencodedQueryComponent . fromIntegral)) <|> pure ""
    return (result key value)

{-# INLINABLE fragment #-}
fragment :: Parser Fragment
fragment =
  labeled "Fragment" $
  (hash *> (Fragment <$> urlEncodedString (C.unencodedFragment . fromIntegral))) <|>
  pure (Fragment mempty)
