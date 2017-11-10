module Iri.Attoparsec.ByteString
where

import Iri.Prelude hiding (foldl)
import Iri.Data
import Data.Attoparsec.ByteString hiding (try)
import qualified Data.Attoparsec.ByteString.Char8 as F
import qualified Data.Text.Punycode as A
import qualified Data.Text.Encoding as B
import qualified Data.Text.Encoding.Error as L
import qualified Data.Vector as D
import qualified Data.ByteString as K
import qualified Iri.PercentEncoding as I
import qualified Iri.PredicateTables as C
import qualified VectorBuilder.MonadPlus as E
import qualified Ptr.Poking as G
import qualified Ptr.ByteString as H
import qualified Text.Builder as J


{-# INLINE takeWhileSatisfiesTable1 #-}
takeWhileSatisfiesTable1 :: Vector Bool -> Parser ByteString
takeWhileSatisfiesTable1 table =
  takeWhile1 (D.unsafeIndex table . fromIntegral)

{-# INLINE percent #-}
percent :: Parser Word8
percent =
  word8 37

{-|
Parser of a well-formed URL conforming to the RFC1738 standard into IRI.
-}
url :: Parser Iri
url =
  do
    parsedScheme <- scheme
    string "://"
    parsedAuthority <- $(todo "")
    parsedHost <- host
    parsedPort <- PresentPort <$> (word8 58 *> port) <|> pure MissingPort
    pathFollows <- True <$ word8 47 <|> pure False
    if pathFollows
      then do
        parsedPath <- path
        $(todo "")
        parsedQuery <- $(todo "")
        parsedFragment <- $(todo "")
        return (Iri parsedScheme parsedAuthority parsedHost parsedPort parsedPath parsedQuery parsedFragment)
      else return (Iri parsedScheme parsedAuthority parsedHost parsedPort (Path mempty) (Query mempty mempty) (Fragment mempty))

{-# INLINE scheme #-}
scheme :: Parser Scheme
scheme =
  fmap (Scheme) (takeWhileSatisfiesTable1 C.scheme)

{-# INLINE host #-}
host :: Parser Host
host =
  $(todo "")

{-# INLINE domainName #-}
domainName :: Parser Idn
domainName =
  fmap Idn (E.sepBy1 domainLabel (word8 46))

{-|
Domain label with Punycode decoding applied.
-}
{-# INLINE domainLabel #-}
domainLabel :: Parser DomainLabel
domainLabel =
  do
    punycode <- takeWhileSatisfiesTable1 C.domainLabel
    case A.decode punycode of
      Right text -> return (DomainLabel text)
      Left exception -> fail (showString "Punycode decoding exception: " (show exception))

{-# INLINE port #-}
port :: Parser Word16
port =
  F.decimal

{-# INLINE path #-}
path :: Parser Path
path =
  fmap Path (E.sepBy pathSegment (word8 47))

{-# INLINE pathSegment #-}
pathSegment :: Parser PathSegment
pathSegment =
  fmap PathSegment urlEncodedSegment

urlEncodedSegment :: Parser Text
urlEncodedSegment =
  foldlMMonadPlus progress (mempty, mempty, B.streamDecodeUtf8) partPoking >>= finish
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
      unencoded <|> encoded
      where
        unencoded =
          takeWhileSatisfiesTable1 C.unencodedPathSegment
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
