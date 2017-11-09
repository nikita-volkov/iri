{-|
Predicate tables for implementing performant predicates.

Reference: <https://tools.ietf.org/html/rfc1738>.
-}
module Iri.CodePointPredicates
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
Only alphanumerics, the special characters "$-_.+!*'(),", and
reserved characters used for their reserved purposes may be used
unencoded within a URL.
-}
unencoded :: Predicate
unencoded =
  special ||| alphanumeric ||| reserved

unencodedPathSegment :: Predicate
unencodedPathSegment =
  alphanumeric ||| special
