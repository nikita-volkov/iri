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
import qualified Iri.Parsing.Text as F
import qualified Iri.Rendering.Text as C
import qualified Iri.Rendering.ByteString as E


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
    testCase "Cyrillic path" $ do
      assertEqual ""
        (Right "https://ru.wikipedia.org/wiki/Баренцбург")
        (fmap C.iri
          (D.uri "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3"))
    ,
    testCase "Complex query" $ do
      assertEqual ""
        (Right "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb")
        (fmap C.iri
          (D.uri "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb"))
    ,
    testCase "Rendering" $
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
    testCase "HTTP URI Text Rendering" $
    assertEqual ""
      "https://ru.wikipedia.org/wiki/Баренцбург"
      (C.httpIri [httpUri|https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3|])
    ,
    testCase "HTTP URI Show" $
    assertEqual ""
      "https://ru.wikipedia.org/wiki/Баренцбург"
      (show [httpUri|https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3|])
    ,
    parsing
  ]

parsing =
  testGroup "Parsing" $
  [
    testCase "Messed up URIs" $
    let
      testUri uri =
        assertEqual "Original and parsed URIs are not equal" (Right uri) (fmap E.uri (D.uri uri))
      in traverse_ testUri
        [
          "http://m.ru.hellomagazine.com/krasota-i-zdorove/ukhod-za-kozhey-i-volosami/11373-5-prichesok-kotorye-sdelayut-vas-molozhe.html?utm_medium=teaser&utm_campaign=gnezdo1&utm_term=15376&utm_content=[TID",
          "https://m.tsargrad.tv/news/komediju_113156?%90\"%20"
        ]
    ,
    testCase "User Info" $
    assertEqual "" (Right "http://user:password@localhost:993") (fmap E.uri (D.uri "http://user:password@localhost:993"))
    ,
    testCase "URI, IRI" $
    assertEqual ""
      (D.uri "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3")
      (F.iri "https://ru.wikipedia.org/wiki/Баренцбург")
    ,
    testGroup "Mess" $
    [
      testCase "1" $
      assertEqual ""
        (Right "https://www.kupivip.ru/catalog/muzhchinam/odezhda?srcid=goo-tm-open&lpu=|source:google|medium:cpc|adposition:1t1|creative:195388077752|term:%2B%D0%B2%D0%B8%D0%BF%20%2B%D0%BA%D1%83%D0%BF%D0%B8|campaignid:801742449|campaign:goo_tm_mob_s_male|other:m|ad:1|&gclid=CjwKEAjwgb3OBRDNi_2TwZ6u7D4SJADsmW8QKiRxDZZ0hRAnyspAcVW582c7zzQSA7CC0FavPR63dBoCrKPw_wcB")
        (fmap E.uri (D.uri "https://www.kupivip.ru/catalog/muzhchinam/odezhda?srcid=goo-tm-open&lpu=|source:google|medium:cpc|adposition:1t1|creative:195388077752|term:%2B%D0%B2%D0%B8%D0%BF%20%2B%D0%BA%D1%83%D0%BF%D0%B8|campaignid:801742449|campaign:goo_tm_mob_s_male|other:m|ad:1|&gclid=CjwKEAjwgb3OBRDNi_2TwZ6u7D4SJADsmW8QKiRxDZZ0hRAnyspAcVW582c7zzQSA7CC0FavPR63dBoCrKPw_wcB"))
      ,
      testCase "2" $
      assertEqual ""
        (Right "https://www.pleer.ru/search_%E4%EB%FF+%E1%E5%EB%FC%FF+%F1%F3%F8%E8%EB%EA%E0.html")
        (fmap E.uri (D.uri "https://www.pleer.ru/search_%E4%EB%FF+%E1%E5%EB%FC%FF+%F1%F3%F8%E8%EB%EA%E0.html"))
    ]
  ]
