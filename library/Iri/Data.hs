{-|
References:

* <https://tools.ietf.org/rfc/rfc3986.txt URI RFC>
* <https://tools.ietf.org/rfc/rfc3987.txt IRI RFC>
-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Iri.Data
where

import Iri.Prelude
import qualified Language.Haskell.TH.Lift as B


{-|
Thorough structure of IRI.
-}
data Iri =
  Iri !Scheme !Hierarchy !Query !Fragment

data RelativeIri =
  RelativeIri !Hierarchy !Query !Fragment

newtype Scheme =
  Scheme ByteString

data Hierarchy =
  AuthorisedHierarchy !Authority !PathSegments |
  AbsoluteHierarchy !PathSegments |
  RelativeHierarchy !PathSegments

data Authority =
  Authority !UserInfo !Host !Port

data UserInfo =
  PresentUserInfo !User !Password |
  MissingUserInfo

newtype User =
  User Text

data Password =
  PresentPassword !Text |
  MissingPassword

data Host =
  NamedHost !RegName |
  IpV4Host !IPv4 |
  IpV6Host !IPv6

newtype RegName =
  RegName (Vector DomainLabel)

newtype DomainLabel =
  DomainLabel Text

data Port =
  PresentPort !Word16 |
  MissingPort

data Path =
  AbsolutePath !PathSegments |
  RelativePath !PathSegments

newtype PathSegments =
  PathSegments (Vector PathSegment)

newtype PathSegment =
  PathSegment Text

{-|
Since the exact structure of the query string is not standardised and
methods used to parse the query string may differ between websites,
we simply represent it as a decoded Unicode string.

See <https://en.wikipedia.org/wiki/Query_string>.
-}
newtype Query =
  Query Text

newtype Fragment =
  Fragment Text


-- * Special cases
-------------------------

-- ** HTTP special case
-------------------------

{-|
HTTP being by far the most common use-case for resource identifiers,
it's been isolated into a dedicated data-type,
which is optimised for that particular case.
-}
data HttpIri =
  HttpIri !Security !Host !Port !PathSegments !Query !Fragment

newtype Security =
  Security Bool

-- ** Web form special case
-------------------------

{-|
A representation of the parsed query according to the web form standard.

See <https://en.wikipedia.org/wiki/Query_string#Web_forms>.
-}
newtype WebFormQuery =
  WebFormQuery (Vector (Text, Vector Text))


-- * Functions
-------------------------

{-| Try to specialize a general IRI to HTTP -}
httpIriFromIri :: Iri -> Either Text HttpIri
httpIriFromIri (Iri (Scheme scheme) hierarchy query fragment) =
  do
    security <- case scheme of
      "http" -> Right (Security False)
      "https" -> Right (Security True)
      _ -> Left ("Not an HTTP scheme: " <> (fromString . show) scheme)
    case hierarchy of
      AuthorisedHierarchy (Authority userInfo host port) pathSegments -> case userInfo of
        MissingUserInfo -> Right (HttpIri security host port pathSegments query fragment)
        PresentUserInfo (User user) _ -> Left ("User Info is present")
      _ -> Left ("Not an authorised hierarchy")

{-| Generalize an HTTP IRI to IRI -}
iriFromHttpIri :: HttpIri -> Iri
iriFromHttpIri (HttpIri (Security secure) host port pathSegments query fragment) =
  Iri scheme hierarchy query fragment
  where
    scheme =
      Scheme (if secure then "https" else "http")
    hierarchy =
      AuthorisedHierarchy (Authority MissingUserInfo host port) pathSegments


-- * Instances
-------------------------

deriving instance Show Iri
deriving instance Eq Iri
B.deriveLift ''Iri

deriving instance Show RelativeIri
deriving instance Eq RelativeIri
B.deriveLift ''RelativeIri

deriving instance Show Scheme
deriving instance Eq Scheme
B.deriveLift ''Scheme

deriving instance Show Hierarchy
deriving instance Eq Hierarchy
B.deriveLift ''Hierarchy

deriving instance Show Authority
deriving instance Eq Authority
B.deriveLift ''Authority

deriving instance Show UserInfo
deriving instance Eq UserInfo
B.deriveLift ''UserInfo

deriving instance Show User
deriving instance Eq User
B.deriveLift ''User

deriving instance Show Password
deriving instance Eq Password
B.deriveLift ''Password

deriving instance Show Host
deriving instance Eq Host
B.deriveLift ''Host

deriving instance Show RegName
deriving instance Eq RegName
B.deriveLift ''RegName

deriving instance Show DomainLabel
deriving instance Eq DomainLabel
B.deriveLift ''DomainLabel

deriving instance Show Port
deriving instance Eq Port
B.deriveLift ''Port

deriving instance Show Path
deriving instance Eq Path
B.deriveLift ''Path

deriving instance Show PathSegments
deriving instance Eq PathSegments
B.deriveLift ''PathSegments

deriving instance Show PathSegment
deriving instance Eq PathSegment
B.deriveLift ''PathSegment

deriving instance Show Query
deriving instance Eq Query
B.deriveLift ''Query

deriving instance Show Fragment
deriving instance Eq Fragment
B.deriveLift ''Fragment

deriving instance Show HttpIri
deriving instance Eq HttpIri
B.deriveLift ''HttpIri

deriving instance Show Security
deriving instance Eq Security
B.deriveLift ''Security

deriving instance Show WebFormQuery
deriving instance Eq WebFormQuery
B.deriveLift ''WebFormQuery

