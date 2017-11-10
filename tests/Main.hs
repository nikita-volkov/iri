module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Iri.Data as A
import qualified Iri.Attoparsec.ByteString as D
import qualified Iri.Rendering.Text as C
import qualified Data.Attoparsec.ByteString as B


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "" $ do
      assertEqual ""
        (Right "https://ru.wikipedia.org/wiki/Баренцбург")
        (fmap C.iri
          (B.parseOnly D.url "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3"))
  ]
