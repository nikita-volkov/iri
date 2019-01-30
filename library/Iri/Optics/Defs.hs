module Iri.Optics.Defs
where

import Iri.Prelude
import Iri.Data
import Iri.Optics.Basics
import qualified Iri.Rendering.ByteString as A
import qualified Iri.Parsing.ByteString as B
import qualified Iri.Rendering.Text as C
import qualified Iri.Parsing.Text as D
import qualified Data.Text.Encoding as Text


-- * Definitions by source
-------------------------

-- ** Text
-------------------------

textIriIri :: Prism' Text Iri
textIriIri =
  prism C.iri (\ text -> either (const (Left text)) Right (D.iri text))

textIriHttpIri :: Prism' Text HttpIri
textIriHttpIri =
  textIriIri . uriHttpIri

-- ** ByteString
-------------------------

byteStringIri :: Prism' ByteString Iri
byteStringIri =
  prism A.uri (\ bytes -> either (const (Left bytes)) Right (B.uri bytes))

byteStringHttpIri :: Prism' ByteString HttpIri
byteStringHttpIri =
  byteStringIri . uriHttpIri

byteStringIriQuery :: Lens' ByteString Query
byteStringIriQuery = undefined

byteStringTextInUtf8 :: Prism' ByteString Text
byteStringTextInUtf8 = prism' Text.encodeUtf8 (either (const Nothing) Just . Text.decodeUtf8')

-- ** URI
-------------------------

uriHttpIri :: Prism' Iri HttpIri
uriHttpIri = prism' iriFromHttpIri (either (const Nothing) Just . httpIriFromIri)

uriScheme :: Lens' Iri Scheme
uriScheme = lens (\ (Iri x _ _ _) -> x) (\ (Iri _ hierarchy query fragment) x -> Iri x hierarchy query fragment)

uriHierarchy :: Lens' Iri Hierarchy
uriHierarchy = lens (\ (Iri _ x _ _) -> x) (\ (Iri scheme _ query fragment) x -> Iri scheme x query fragment)

uriQuery :: Lens' Iri Query
uriQuery = lens (\ (Iri _ _ x _) -> x) (\ (Iri scheme hierarchy _ fragment) x -> Iri scheme hierarchy x fragment)

uriParametricQuery :: Traversal' Iri ParametricQuery
uriParametricQuery = uriQuery . queryParametricQuery

uriUnicodeParametricQueryInUtf8 :: Traversal' Iri UnicodeParametricQuery
uriUnicodeParametricQueryInUtf8 = uriQuery . queryUnicodeParametricQueryInUtf8

uriFragment :: Lens' Iri Fragment
uriFragment = lens (\ (Iri _ _ _ x) -> x) (\ (Iri scheme hierarchy query _) x -> Iri scheme hierarchy query x)

-- ** Scheme
-------------------------

schemeByteString :: Lens' Scheme ByteString
schemeByteString = lens (\ (Scheme x) -> x) (const Scheme)

-- ** Query
-------------------------

queryParametricQuery :: Prism' Query ParametricQuery
queryParametricQuery = undefined

queryUnicodeParametricQueryInUtf8 :: Prism' Query UnicodeParametricQuery
queryUnicodeParametricQueryInUtf8 = queryParametricQuery . parametricQueryUnicodeParametricQueryInUtf8

-- ** Parametric query
-------------------------

parametricQueryUnicodeParametricQueryInUtf8 :: Prism' ParametricQuery UnicodeParametricQuery
parametricQueryUnicodeParametricQueryInUtf8 = undefined

-- ** Fragment
-------------------------

fragmentByteString :: Lens' Fragment ByteString
fragmentByteString = lens (\ (Fragment x) -> x) (const Fragment)
