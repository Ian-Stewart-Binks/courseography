module DiagramGenerator where

import Data.Text.Lazy (Text, pack, unpack)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Control.Monad
import Data.GraphViz 
import Data.GraphViz.Printing (toDot, renderDot)
import Control.Monad.IO.Class  (liftIO)
import Data.GraphViz.Attributes.Complete

schema :: Gr Text Text
schema = mkGraph [(1, pack "Lectures"),
                  (2, pack "Tutorials"),
                  (3, pack "Tutorials"),
                  (4, pack "Else")]
                 [(1, 3, pack "")]

params :: GraphvizParams n Text Text () Text
params = nonClusteredParams {
  globalAttributes = ga,
  fmtNode = fn,
  fmtEdge = fe
  }
  where
    ga = [
      GraphAttrs [
         RankDir FromLeft,
         BgColor [toWColor Transparent]
         ],
      NodeAttrs [
        Shape BoxShape,
        FillColor [toWColor White],
        Style [SItem Filled []]
        ]
      ]

    fn (nodeID, nodeLabel) = [(Label . StrLabel) nodeLabel, Width 20]
    fe (source, target, edgeID) = [(Label . StrLabel) edgeID]

main :: IO ()
main = do
	    void $ runGraphviz (graphToDot params schema) Svg "gg.g"
