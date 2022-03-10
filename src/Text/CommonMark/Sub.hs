{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Text.CommonMark.Sub
    ( HeadingPattern (..)
    , HeadingTitlePattern (..)
    , Level
    , extractSection
    , flattenInlineNodes
    , matchesHeading
    ) where

import Prelude hiding (concat)

import CMark
import Data.Text (Text, concat, pack, strip, toLower)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

data HeadingPattern = HeadingPattern
    { headingLevel :: Level
    , caseInsensitive :: Bool
    , titlePattern :: HeadingTitlePattern
    } deriving (Eq, Ord, Show)

data HeadingTitlePattern
    = HeadingTitleRegex Text
    | HeadingTitleText Text
    deriving (Eq, Ord, Show)

extractSection :: HeadingPattern -> Node -> Node
extractSection pat@HeadingPattern { headingLevel } (Node pos DOCUMENT nodes) =
    Node pos DOCUMENT $ case headlessNodes of
        (x : xs) -> x : takeWhile isInSection xs
        [] -> []
  where
    isInSection :: Node -> Bool
    isInSection (Node _ (HEADING lv) _) = lv > headingLevel
    isInSection _ = True
    headlessNodes :: [Node]
    headlessNodes =
        dropWhile (not . matchesHeading pat) nodes
extractSection _ (Node pos _ _) = Node pos DOCUMENT []

matchesHeading :: HeadingPattern -> Node -> Bool
matchesHeading HeadingPattern { headingLevel
                              , caseInsensitive
                              , titlePattern
                              } = case titlePattern of
    HeadingTitleText text -> if caseInsensitive
        then \ case
            (Node _ (HEADING lv) nodes) ->
                lv == headingLevel &&
                    toLower (flattenInlineNodes nodes) == toLower text
            _ -> False
        else \ case
            (Node _ (HEADING lv) nodes) ->
                lv == headingLevel && flattenInlineNodes nodes == text
            _ -> False
    HeadingTitleRegex regexPat ->
        let compOption = CompOption
                { caseSensitive = not caseInsensitive
                , multiline = False
                , rightAssoc = True
                , newSyntax = True
                , lastStarGreedy = True
                }
            execOption = ExecOption { captureGroups = False }
            regex' = compile compOption execOption regexPat
        in
            case regex' of
                Left _ -> const False
                Right regex ->
                    \ case
                    (Node _ (HEADING lv) nodes) ->
                        lv == headingLevel &&
                            case execute regex (flattenInlineNodes nodes) of
                                Right (Just _) -> True
                                _ -> False
                    _ -> False

flattenInlineNodes :: [Node] -> Text
flattenInlineNodes =
    strip . concat . map flatten
  where
    flatten :: Node -> Text
    flatten (Node _ (TEXT text) _) = text
    flatten (Node _ LINEBREAK _) = pack "\n"
    flatten (Node _ (HTML_INLINE text) _) = text
    flatten (Node _ (CODE text) _) = text
    flatten (Node _ _ nodes) = flattenInlineNodes nodes
