module Iri.Rendering.Text.Internal where

import Iri.Data.Types
import Iri.Prelude
import Iri.Rendering.TextBuilder.Internal qualified as A
import Text.Builder qualified as B

-- | Render as a Unicode IRI text
iri :: Iri -> Text
iri = B.run . A.iri

-- | Render as a Unicode IRI text
httpIri :: HttpIri -> Text
httpIri = B.run . A.httpIri

scheme :: Scheme -> Text
scheme = B.run . A.scheme

hierarchy :: Hierarchy -> Text
hierarchy = B.run . A.hierarchy

authority :: Authority -> Text
authority = B.run . A.authority

userInfo :: UserInfo -> Text
userInfo = B.run . A.userInfo

host :: Host -> Text
host = B.run . A.host

regName :: RegName -> Text
regName = B.run . A.regName

domainLabel :: DomainLabel -> Text
domainLabel = B.run . A.domainLabel

ipV4 :: IPv4 -> Text
ipV4 = B.run . A.ipV4

ipV6 :: IPv6 -> Text
ipV6 = B.run . A.ipV6

port :: Port -> Text
port = B.run . A.port

path :: Path -> Text
path = B.run . A.path

pathSegment :: PathSegment -> Text
pathSegment = B.run . A.pathSegment

query :: Query -> Text
query = B.run . A.query

fragment :: Fragment -> Text
fragment = B.run . A.fragment
