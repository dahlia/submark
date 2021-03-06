{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.CommonMark.SubSpec (spec) where

import Data.Text
import Test.Hspec

import CMark (Node)
import Text.CommonMark.QQ
import Text.CommonMark.Sub

ciEq :: Text -> Text -> Bool
ciEq t t' = toLower t == toLower t'

source :: Node
source = [doc|Doc title
=======================

Lorem ipsum dolor sit amet, ne omnes vitae mel, alterum invenire duo ut,
ad deterruisset comprehensam qui. At ius ludus iracundia vituperatoribus.

Section to *extract*
--------------------

Ei nec prima congue noster, ut vis ipsum concludaturque.
Nisl modus dolorum sea cu, qui et stet decore putant.

> Qui vivendo consectetuer id, ea suas laoreet adipisci sed.
> Mel omnes partiendo in, id sed soluta lucilius. Amet cibo est an,
> cu sed placerat maluisset. Vel et esse fugit commune, at quo sonet facilisi.

### Subsection

Vis ut audire inimicus, ut mea putant prompta.
Vivendo assentior mnesarchum ius ei, novum aperiam habemus est in,
ea agam putant consectetuer sed.

Other section
-------------

Pro fuisset deserunt consulatu at, no epicuri quaestio vix,
case scripserit at vis. No eam alia facilisis. At quo oratio omnium,
nibh malis dolores an eam.
|]

expected :: Node
expected = [doc|
Section to *extract*
--------------------

Ei nec prima congue noster, ut vis ipsum concludaturque.
Nisl modus dolorum sea cu, qui et stet decore putant.

> Qui vivendo consectetuer id, ea suas laoreet adipisci sed.
> Mel omnes partiendo in, id sed soluta lucilius. Amet cibo est an,
> cu sed placerat maluisset. Vel et esse fugit commune, at quo sonet facilisi.

### Subsection

Vis ut audire inimicus, ut mea putant prompta.
Vivendo assentior mnesarchum ius ei, novum aperiam habemus est in,
ea agam putant consectetuer sed.

|]

spec :: Spec
spec = do
    specify "extractSection" $ do
        extractSection 2 (==) "Section to extract" source `shouldBe` expected
        extractSection 3 (==) "Section to extract" source `shouldNotBe` expected
        extractSection 2 (==) "section to extract" source `shouldNotBe` expected
        extractSection 3 (==) "section to extract" source `shouldNotBe` expected
        -- Case insensitivity
        extractSection 2 ciEq "Section to extract" source `shouldBe` expected
        extractSection 3 ciEq "Section to extract" source `shouldNotBe` expected
        extractSection 2 ciEq "section to extract" source `shouldBe` expected
        extractSection 3 ciEq "section to extract" source `shouldNotBe` expected
    specify "matchesHeading" $ do
        [node|# Title|] `shouldSatisfy` matchesHeading 1 (==) "Title"
        [node|### Title|] `shouldSatisfy` matchesHeading 3 (==) "Title"
        [node|# Other title|] `shouldSatisfy`
            matchesHeading 1 (==) "Other title"
        [node|# Complex *title*|] `shouldSatisfy`
            matchesHeading 1 (==) "Complex title"
        [node|# Title|] `shouldNotSatisfy` matchesHeading 2 (==) "Title"
        [node|# Title|] `shouldNotSatisfy` matchesHeading 1 (==) "Wrong title"
        [node|# Title|] `shouldNotSatisfy` matchesHeading 1 (==) "title"
        -- Case insensitivity
        [node|# Title|] `shouldNotSatisfy` matchesHeading 1 (==) "title"
        [node|# Title|] `shouldSatisfy` matchesHeading 1 ciEq "title"
        [node|# Title|] `shouldNotSatisfy` matchesHeading 2 (==) "title"
        [node|# Title|] `shouldNotSatisfy` matchesHeading 2 ciEq "title"
    specify "flattenInlineNodes" $ do
        flattenInlineNodes [nodes|*Test* nodes.|] `shouldBe` "Test nodes."
        flattenInlineNodes [nodes|Testing multiple paragraphs.

[Link](http://example.com/) test.|] `shouldBe`
            "Testing multiple paragraphs.Link test."
