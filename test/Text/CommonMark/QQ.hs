{-# LANGUAGE TemplateHaskell #-}
module Text.CommonMark.QQ (doc, node, nodes) where

import CMark
import Data.Text hiding (head)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

doc :: QuasiQuoter
doc = QuasiQuoter
    docQ
    (error "Cannot use [doc|...|] as a pattern")
    (error "Cannot use [doc|...|] as a type")
    (error "Cannot use [doc|...|] as a dec")

nodes :: QuasiQuoter
nodes = QuasiQuoter
    nodesQ
    (error "Cannot use [nodes|...|] as a pattern")
    (error "Cannot use [nodes|...|] as a type")
    (error "Cannot use [nodes|...|] as a dec")

node :: QuasiQuoter
node = QuasiQuoter
    nodeQ
    (error "Cannot use [node|...|] as a pattern")
    (error "Cannot use [node|...|] as a type")
    (error "Cannot use [node|...|] as a dec")

parse :: String -> Node
parse = dropPosInfo . commonmarkToNode [optNormalize] . pack

dropPosInfo :: Node -> Node
dropPosInfo (Node _ nodeType children) =
    Node Nothing nodeType $ fmap dropPosInfo children

docQ :: String -> TH.ExpQ
docQ text = [e|parse text|]

nodesQ :: String -> TH.ExpQ
nodesQ text = [e|let (Node _ _ ns) = parse text in ns|]

nodeQ :: String -> TH.ExpQ
nodeQ text = [e|let (Node _ _ ns) = parse text in head ns|]
