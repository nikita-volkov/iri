module Iri.Instances.Eq
where

import Iri.Prelude
import Iri.Data


deriving instance Eq Iri

deriving instance Eq Scheme

deriving instance Eq Hierarchy

deriving instance Eq Authority

deriving instance Eq UserInfo

deriving instance Eq User

deriving instance Eq Password

deriving instance Eq Host

deriving instance Eq RegName

deriving instance Eq DomainLabel

deriving instance Eq Port

deriving instance Eq PathSegments

deriving instance Eq PathSegment

deriving instance Eq Query

deriving instance Eq Fragment

deriving instance Eq HttpIri

deriving instance Eq Security
