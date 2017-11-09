module Iri.Data
where

import Iri.Prelude
import qualified Data.Vector.Unboxed as A


{-|
Space-efficient representation of IRI.

For reference, see <https://en.wikipedia.org/wiki/Internationalized_Resource_Identifier>.
-}
data Iri =
  Iri
    {-# UNPACK #-} !Scheme
    {-# UNPACK #-} !Authority
    {-# UNPACK #-} !Host
    {-# UNPACK #-} !Port
    {-# UNPACK #-} !Path
    {-# UNPACK #-} !Query
    {-# UNPACK #-} !Fragment

newtype Scheme =
  Scheme Utf8String

data Authority =
  PresentAuthority {-# UNPACK #-} !User {-# UNPACK #-} !Password |
  MissingAuthority

newtype User =
  User Utf8String

data Password =
  PresentPassword {-# UNPACK #-} !Utf8String |
  MissingPassword

data Host =
  NamedHost {-# UNPACK #-} !Idn |
  IpV4Host {-# UNPACK #-} !IPv4 |
  IpV6Host {-# UNPACK #-} !IPv6

{-|
Internationalized domain name.

For reference, see <https://en.wikipedia.org/wiki/Internationalized_domain_name>.
-}
newtype Idn =
  Idn (Vector DomainLabel)

newtype DomainLabel =
  DomainLabel Utf8String

data Port =
  PresentPort {-# UNPACK #-} !Word16 |
  MissingPort

newtype Path =
  Path (Vector PathSegment)

newtype PathSegment =
  PathSegment Utf8String

data Query =
  Query {-# UNPACK #-} !(Vector Utf8String) {-# UNPACK #-} !(Vector Utf8String)

newtype Fragment =
  Fragment Utf8String

{-|
UTF8-encoded bytes.

Representing ASCII data comes at no cost. URI is encoded in ASCII.
-}
newtype Utf8String =
  Utf8String ByteString
