module Iri.Data.Instances.Ord
where

import Iri.Prelude
import Iri.Data.Types
import Iri.Data.Instances.Eq


deriving instance Ord Iri

deriving instance Ord Scheme

deriving instance Ord Hierarchy

deriving instance Ord Authority

deriving instance Ord UserInfo

deriving instance Ord User

deriving instance Ord Password

deriving instance Ord Host

deriving instance Ord RegName

deriving instance Ord DomainLabel

deriving instance Ord Port

deriving instance Ord Path

deriving instance Ord PathSegment

deriving instance Ord Query

deriving instance Ord Fragment

deriving instance Ord HttpIri

deriving instance Ord Security
