{-|
Predicates.

Reference:
<https://www.ietf.org/rfc/rfc3986 RFC3986: Uniform Resource Identifier (URI): Generic Syntax>
-}
module Iri.Rfc3986.CodePointPredicates
where

import Iri.Prelude hiding ((|||), (&&&), inRange)
import qualified Data.Vector as A


type Predicate =
  Int -> Bool

-- * Helpers
-------------------------

oneOfChars :: [Char] -> Predicate
oneOfChars string i =
  elem i (fmap ord string)

(|||) :: Predicate -> Predicate -> Predicate
(|||) left right i =
  left i || right i

(&&&) :: Predicate -> Predicate -> Predicate
(&&&) left right i =
  left i && right i

inRange :: Int -> Int -> Predicate
inRange min max i =
  i >= min && i <= max

inCharRange :: Char -> Char -> Predicate
inCharRange min max =
  inRange (ord min) (ord max)

-- * Specifics
-------------------------

alphanumeric :: Predicate
alphanumeric =
  inCharRange 'a' 'z' |||
  inCharRange 'A' 'Z' |||
  inCharRange '0' '9'

scheme :: Predicate
scheme i =
  i >= ord 'a' && i <= ord 'z' ||
  i >= ord 'A' && i <= ord 'Z' ||
  i >= ord '0' && i <= ord '9' ||
  i == ord '+' ||
  i == ord '.' ||
  i == ord '-'

domainLabel :: Predicate
domainLabel i =
  i >= ord 'a' && i <= ord 'z' ||
  i >= ord 'A' && i <= ord 'Z' ||
  i >= ord '0' && i <= ord '9' ||
  i == ord '-' ||
  i == ord '_'

encoded :: Predicate
encoded =
  nonGraphicAscii ||| unsafe

{-|
URLs are written only with the graphic printable characters of the
US-ASCII coded character set. The octets 80-FF hexadecimal are not
used in US-ASCII, and the octets 00-1F and 7F hexadecimal represent
control characters; these must be encoded.
-}
nonGraphicAscii :: Predicate
nonGraphicAscii =
  octal ||| control

-- 8-bit
octal :: Predicate
octal i =
  i >= 0x80 && i <= 0xFF 

control :: Predicate
control i =
  i <= 0x1F ||
  i == 0x7F

{-|
Characters can be unsafe for a number of reasons.  The space
character is unsafe because significant spaces may disappear and
insignificant spaces may be introduced when URLs are transcribed or
typeset or subjected to the treatment of word-processing programs.
The characters "<" and ">" are unsafe because they are used as the
delimiters around URLs in free text; the quote mark (""") is used to
delimit URLs in some systems.  The character "#" is unsafe and should
always be encoded because it is used in World Wide Web and in other
systems to delimit a URL from a fragment/anchor identifier that might
follow it.  The character "%" is unsafe because it is used for
encodings of other characters.  Other characters are unsafe because
gateways and other transport agents are known to sometimes modify
such characters. These characters are "{", "}", "|", "\", "^", "~",
"[", "]", and "`".
-}
unsafe :: Predicate
unsafe =
  oneOfChars " <>\"#%{}|\\^~[]`"

{-|
Many URL schemes reserve certain characters for a special meaning:
their appearance in the scheme-specific part of the URL has a
designated semantics. If the character corresponding to an octet is
reserved in a scheme, the octet must be encoded.  The characters ";",
"/", "?", ":", "@", "=" and "&" are the characters which may be
reserved for special meaning within a scheme. No other characters may
be reserved within a scheme.
-}
reserved :: Predicate
reserved =
  oneOfChars ";/?:@=&"

special :: Predicate
special =
  oneOfChars "$-_.+!*'(),"

{-|
The path component contains data, usually organized in hierarchical
form, that, along with data in the non-hierarchical query component
(Section 3.4), serves to identify a resource within the scope of the
URI's scheme and naming authority (if any).  The path is terminated
by the first question mark ("?") or number sign ("#") character, or
by the end of the URI.

If a URI contains an authority component, then the path component
must either be empty or begin with a slash ("/") character.  If a URI
does not contain an authority component, then the path cannot begin
with two slash characters ("//").  In addition, a URI reference
(Section 4.1) may be a relative-path reference, in which case the
first path segment cannot contain a colon (":") character.  The ABNF
requires five separate rules to disambiguate these cases, only one of
which will match the path substring within a given URI reference.  We
use the generic term "path component" to describe the URI substring
matched by the parser to one of these rules.

  path          = path-abempty    ; begins with "/" or is empty
                / path-absolute   ; begins with "/" but not "//"
                / path-noscheme   ; begins with a non-colon segment
                / path-rootless   ; begins with a segment
                / path-empty      ; zero characters

  path-abempty  = *( "/" segment )
  path-absolute = "/" [ segment-nz *( "/" segment ) ]
  path-noscheme = segment-nz-nc *( "/" segment )
  path-rootless = segment-nz *( "/" segment )
  path-empty    = 0<pchar>
  segment       = *pchar
  segment-nz    = 1*pchar
  segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
                ; non-zero-length segment without any colon ":"

  pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

A path consists of a sequence of path segments separated by a slash
("/") character.  A path is always defined for a URI, though the
defined path may be empty (zero length).  Use of the slash character
to indicate hierarchy is only required when a URI will be used as the
context for relative references.  For example, the URI
<mailto:fred@example.com> has a path of "fred@example.com", whereas
the URI <foo://info.example.com?fred> has an empty path.

The path segments "." and "..", also known as dot-segments, are
defined for relative reference within the path name hierarchy.  They
are intended for use at the beginning of a relative-path reference
(Section 4.2) to indicate relative position within the hierarchical
tree of names.  This is similar to their role within some operating
systems' file directory structures to indicate the current directory
and parent directory, respectively.  However, unlike in a file
system, these dot-segments are only interpreted within the URI path
hierarchy and are removed as part of the resolution process (Section
5.2).

Aside from dot-segments in hierarchical paths, a path segment is
considered opaque by the generic syntax.  URI producing applications
often use the reserved characters allowed in a segment to delimit
scheme-specific or dereference-handler-specific subcomponents.  For
example, the semicolon (";") and equals ("=") reserved characters are
often used to delimit parameters and parameter values applicable to
that segment.  The comma (",") reserved character is often used for
similar purposes.  For example, one URI producer might use a segment
such as "name;v=1.1" to indicate a reference to version 1.1 of
"name", whereas another might use a segment such as "name,1.1" to
indicate the same.  Parameter types may be defined by scheme-specific
semantics, but in most cases the syntax of a parameter is specific to
the implementation of the URI's dereferencing algorithm.

<https://tools.ietf.org/html/rfc3986#section-3.3>
-}
unencodedPathSegment :: Predicate
unencodedPathSegment =
  alphanumeric ||| special

{-|
The query component contains non-hierarchical data that, along with
data in the path component (Section 3.3), serves to identify a
resource within the scope of the URI's scheme and naming authority
(if any).  The query component is indicated by the first question
mark ("?") character and terminated by a number sign ("#") character
or by the end of the URI.

  query       = *( pchar / "/" / "?" )

The characters slash ("/") and question mark ("?") may represent data
within the query component.  Beware that some older, erroneous
implementations may not handle such data correctly when it is used as
the base URI for relative references (Section 5.1), apparently
because they fail to distinguish query data from path data when
looking for hierarchical separators.  However, as query components
are often used to carry identifying information in the form of
"key=value" pairs and one frequently used value is a reference to
another URI, it is sometimes better for usability to avoid percent-
encoding those characters.

<https://tools.ietf.org/html/rfc3986#section-3.4>
-}
unencodedQueryComponent :: Predicate
unencodedQueryComponent =
  alphanumeric ||| special ||| oneOfChars "/?"

unencodedFragment :: Predicate
unencodedFragment =
  unencodedQueryComponent
