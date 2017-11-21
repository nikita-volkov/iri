module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Iri.QuasiQuoter
import qualified Iri.Data as A
import qualified Iri.Parsing.ByteString as D
import qualified Iri.Rendering.Text as C
import qualified Iri.Rendering.ByteString.Ascii as E


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "No path" $
    assertEqual ""
      (Right "https://ru.wikipedia.org?query")
      (fmap C.iri
        (D.uri "https://ru.wikipedia.org?query"))
    ,
    testCase "" $ do
      assertEqual ""
        (Right "https://ru.wikipedia.org/wiki/Баренцбург")
        (fmap C.iri
          (D.uri "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3"))
    ,
    testCase "" $ do
      assertEqual ""
        (Right "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb")
        (fmap C.iri
          (D.uri "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb"))
    ,
    testCase "" $
    let
      inputs =
        [
          "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3"
          ,
          "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb"
        ]
      in
        forM_ inputs $ \ input ->
        let
          Right iri = D.uri input
          in assertEqual "" input (E.uri iri)
    ,
    testCase "about:blank" $ assertEqual "" "about:blank" (E.uri [uri|about:blank|])
    ,
    testCase "Empty hierarchy" $ assertEqual "" "about:" (E.uri [uri|about:|])
    ,
    testCase "HTTP URI" $
    assertEqual ""
      "https://ru.wikipedia.org/wiki/Баренцбург"
      (C.httpIri [httpUri|https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3|])
  ]

