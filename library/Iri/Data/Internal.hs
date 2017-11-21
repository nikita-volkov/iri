{-|
References:

* <https://tools.ietf.org/rfc/rfc3986.txt URI RFC>
* <https://tools.ietf.org/rfc/rfc3987.txt IRI RFC>
-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Iri.Data.Internal
where

import Iri.Prelude


{-|
Thorough structure of IRI.
-}
data Iri =
  Iri !Scheme !Hierarchy !Query !Fragment

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

Compared to the general IRI definition it:

* only supports the HTTP and HTTPS schemes
* misses the Username and Password components
* requires the Host component
* requires the Path component to be absolute
-}
data HttpIri =
  HttpIri !Security !Host !Port !PathSegments !Query !Fragment

newtype Security =
  Security Bool


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
