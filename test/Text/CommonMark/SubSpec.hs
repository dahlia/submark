{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.CommonMark.SubSpec (spec) where

import Test.Hspec

import CMark (Node)
import Text.CommonMark.QQ
import Text.CommonMark.Sub

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
    describe "extractSection" $ do
        specify "w/ HeadingTitleText" $ do
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Section to extract"
                    }
                source
                `shouldBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Section to extract"
                    }
                source
                `shouldNotBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "section to extract"
                    }
                source
                `shouldNotBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "section to extract"
                    }
                source
                `shouldNotBe` expected
            -- Case insensitivity
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleText "Section to extract"
                    }
                source
                `shouldBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleText "Section to extract"
                    }
                source
                `shouldNotBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleText "section to extract"
                    }
                source
                `shouldBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleText "section to extract"
                    }
                source
                `shouldNotBe` expected
        specify "w/ HeadingTitleRegex" $ do
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "to[[:space:]]+extract$"
                    }
                source
                `shouldBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "to[[:space:]]+extract$"
                    }
                source
                `shouldNotBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "TO[[:space:]]+EXTRACT$"
                    }
                source
                `shouldNotBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "TO[[:space:]]+EXTRACT$"
                    }
                source
                `shouldNotBe` expected
            -- Case insensitivity
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleRegex "to[[:space:]]+extract$"
                    }
                source
                `shouldBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleRegex "to[[:space:]]+extract$"
                    }
                source
                `shouldNotBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleRegex "TO[[:space:]]+EXTRACT$"
                    }
                source
                `shouldBe` expected
            extractSection
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleRegex "TO[[:space:]]+EXTRACT$"
                    }
                source
                `shouldNotBe` expected
    describe "matchesHeading" $ do
        specify "w/ HeadingTitleText" $ do
            [node|# Title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Title"
                    }
            [node|### Title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Title"
                    }
            [node|# Other title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Other title"
                    }
            [node|# Complex *title*|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Complex title"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Title"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "Wrong title"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleText "title"
                    }
            -- Case insensitivity
            [node|# Title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleText "title"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleText "title"
                    }
        specify "w/ HeadingTitleRegex" $ do
            [node|# Title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "^Title$"
                    }
            [node|### Title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 3
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "^Title$"
                    }
            [node|# Other title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern =
                        HeadingTitleRegex "^Other[[:space:]]+title$"
                    }
            [node|# Complex *title*|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern =
                        HeadingTitleRegex "^Complex[[:space:]]+title$"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "^Title$"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern =
                        HeadingTitleRegex "^Wrong[[:space:]]+title$"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = False
                    , titlePattern = HeadingTitleRegex "$title$"
                    }
            -- Case insensitivity
            [node|# Title|] `shouldSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 1
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleRegex "^title$"
                    }
            [node|# Title|] `shouldNotSatisfy` matchesHeading
                HeadingPattern
                    { headingLevel = 2
                    , caseInsensitive = True
                    , titlePattern = HeadingTitleRegex "^title$"
                    }
    specify "flattenInlineNodes" $ do
        flattenInlineNodes [nodes|*Test* nodes.|] `shouldBe` "Test nodes."
        flattenInlineNodes [nodes|Testing multiple paragraphs.

[Link](http://example.com/) test.|] `shouldBe`
            "Testing multiple paragraphs.Link test."
