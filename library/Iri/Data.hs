module Iri.Data
where

import Iri.Prelude
import qualified Language.Haskell.TH.Lift as B


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
  Query (Vector (Text, Text))

newtype Fragment =
  Fragment Text

-- * Instances
-------------------------

deriving instance Show Iri
deriving instance Eq Iri
B.deriveLift ''Iri

deriving instance Show Scheme
deriving instance Eq Scheme
B.deriveLift ''Scheme

deriving instance Show Authority
deriving instance Eq Authority
B.deriveLift ''Authority

deriving instance Show User
deriving instance Eq User
B.deriveLift ''User

deriving instance Show Password
deriving instance Eq Password
B.deriveLift ''Password

deriving instance Show Host
deriving instance Eq Host
B.deriveLift ''Host

deriving instance Show Idn
deriving instance Eq Idn
B.deriveLift ''Idn

deriving instance Show DomainLabel
deriving instance Eq DomainLabel
B.deriveLift ''DomainLabel

deriving instance Show Port
deriving instance Eq Port
B.deriveLift ''Port

deriving instance Show Path
deriving instance Eq Path
B.deriveLift ''Path

deriving instance Show PathSegment
deriving instance Eq PathSegment
B.deriveLift ''PathSegment

deriving instance Show Query
deriving instance Eq Query
B.deriveLift ''Query

deriving instance Show Fragment
deriving instance Eq Fragment
B.deriveLift ''Fragment
