module Text.CommonMark.Sub
    ( Level
    , extractSection
    , flattenInlineNodes
    , matchesHeading
    ) where

import Prelude hiding (concat)

import CMark
import Data.Text (Text, concat, pack, strip)

extractSection :: Level -> (Text -> Text -> Bool) -> Text -> Node -> Node
extractSection headingLevel equals headingTitle (Node pos DOCUMENT nodes) =
    Node pos DOCUMENT $ case headlessNodes of
        (x : xs) -> x : takeWhile (isInSection headingLevel) xs
        [] -> []
  where
    isInSection :: Level -> Node -> Bool
    isInSection level (Node _ (HEADING lv) _) = lv > level
    isInSection _ _ = True
    headlessNodes :: [Node]
    headlessNodes =
        dropWhile (not . matchesHeading headingLevel equals headingTitle) nodes
extractSection _ _ _ (Node pos _ _) = Node pos DOCUMENT []

matchesHeading :: Level -> (Text -> Text -> Bool) -> Text -> Node -> Bool
matchesHeading level equals text (Node _ (HEADING lv) nodes) =
    lv == level && flattenInlineNodes nodes `equals` text
matchesHeading _ _ _ _ = False

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
