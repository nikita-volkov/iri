module Iri.Data
where

import Iri.Prelude
import qualified Data.Vector.Unboxed as A


{-|
Thorough structure of IRI.

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
  Scheme ByteString

data Authority =
  PresentAuthority {-# UNPACK #-} !User {-# UNPACK #-} !Password |
  MissingAuthority

newtype User =
  User Text

data Password =
  PresentPassword {-# UNPACK #-} !Text |
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
  DomainLabel Text

data Port =
  PresentPort {-# UNPACK #-} !Word16 |
  MissingPort

newtype Path =
  Path (Vector PathSegment)

newtype PathSegment =
  PathSegment Text

data Query =
  Query {-# UNPACK #-} !(Vector Text) {-# UNPACK #-} !(Vector Text)

newtype Fragment =
  Fragment Text
