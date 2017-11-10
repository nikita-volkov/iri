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
    ,
    testCase "" $ do
      assertEqual ""
        (Right "https://ru.wikipedia.org/wiki/Баренцбург")
        (fmap C.iri
          (B.parseOnly D.url "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb"))
  ]
