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

newtype Query =
  Query (HashMap Text (Vector Text))

newtype Fragment =
  Fragment Text

-- * Instances
-------------------------

deriving instance Show Iri
deriving instance Eq Iri

deriving instance Show Scheme
deriving instance Eq Scheme

deriving instance Show Authority
deriving instance Eq Authority

deriving instance Show User
deriving instance Eq User

deriving instance Show Password
deriving instance Eq Password

deriving instance Show Host
deriving instance Eq Host

deriving instance Show Idn
deriving instance Eq Idn

deriving instance Show DomainLabel
deriving instance Eq DomainLabel

deriving instance Show Port
deriving instance Eq Port

deriving instance Show Path
deriving instance Eq Path

deriving instance Show PathSegment
deriving instance Eq PathSegment

deriving instance Show Query
deriving instance Eq Query

deriving instance Show Fragment
deriving instance Eq Fragment
