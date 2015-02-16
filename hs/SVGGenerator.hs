{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module SVGGenerator where

import SVGTypes
import Tables
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Data.Char
import Data.Conduit
import Data.List.Split
import Data.List
import JsonParser
import ParserUtil
import SVGBuilder

-- | The SVG tag for an SVG document, along with an opening 'g' tag.
svgHeader :: String
svgHeader = 
    "<svg" ++
    " xmlns:dc=\"http://purl.org/dc/elements/1.1/\"" ++
    " xmlns:cc=\"http://creativecommons.org/ns#\"" ++
    " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"" ++
    " xmlns:svg=\"http://www.w3.org/2000/svg\"" ++
    " xmlns=\"http://www.w3.org/2000/svg\"" ++
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\"" ++
    " xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"" ++
    " xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"" ++
    " width=\"1078\"" ++
    " height=\"1227\"" ++
    " stroke=\"black\"" ++
    " version=\"1.1\"" ++
    " sodipodi:docname=\"graph_regions.svg\"><defs>" ++
    "     <marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"1\" refY=\"5\" markerUnits=\"strokeWidth\" orient=\"auto\" markerWidth=\"7\" markerHeight=\"7\">" ++
    "       <polyline points=\"0,1 10,5 0,9\" fill=\"black\"></polyline>" ++
    "     </marker>" ++
    "   </defs><g>"

-- | A closing 'g' tag followed by a closing 'svg' tag.
svgFooter :: String
svgFooter = "</g></svg>"

-- | Builds an SVG document.
buildSVG :: IO ()
buildSVG = 
    runSqlite dbStr $ do
        sqlRects    :: [Entity Rects]    <- selectList [] []
        sqlTexts    :: [Entity Texts]    <- selectList [] []
        sqlPaths    :: [Entity Paths]    <- selectList [] []
        sqlEllipses :: [Entity Ellipses] <- selectList [] []

        let texts      = map (buildText . entityVal) sqlTexts
        let paths      = buildPaths 0 $ map entityVal sqlPaths
        let regions    = filter pathIsRegion paths
        let edges      = filter (not . pathIsRegion) paths
        let rects      = map (buildRect texts . entityVal) sqlRects
        let ellipses   = buildEllipses texts 0 $ map entityVal sqlEllipses

        let processedEdges = map (processPath rects ellipses) edges
        let processedRects = map (processRect processedEdges) rects
        let processedEllipses = map (processEllipse processedEdges) ellipses

        let rectXml    = map convertRectToXML processedRects

        let textXml    = map (convertTextToXML . buildText . entityVal) sqlTexts
        let edgeXml    = map convertPathToXML processedEdges
        let regionXml  = map convertRegionToXML regions
        let ellipseXml = map convertEllipseToXML processedEllipses

        liftIO $ writeHeader
        liftIO $ writeRegions $ unwords regionXml
        liftIO $ writeEdges $ unwords edgeXml
        liftIO $ writeRects $ unwords rectXml
        liftIO $ writeEllipses $ unwords ellipseXml
        liftIO $ writeFooter

-- | Writes the SVG header to the output file.
writeHeader :: IO ()
writeHeader = writeFile "Testfile.svg" svgHeader

-- | Writes the SVG footer to the output file.
writeFooter :: IO ()
writeFooter = appendFile "Testfile.svg" svgFooter

-- | Writes the `rect` section to the output file.
writeRects :: String -> IO ()
writeRects rectXml = do
    appendFile "Testfile.svg" "<g>"
    appendFile "Testfile.svg" rectXml
    appendFile "Testfile.svg" "</g>"

-- | Writes the `ellipse` section to the output file.
writeEllipses :: String -> IO ()
writeEllipses ellipseXml = do
    appendFile "Testfile.svg" "<g>"
    appendFile "Testfile.svg" ellipseXml
    appendFile "Testfile.svg" "</g>"

-- | Writes the `region` section to the output file.
writeRegions :: String -> IO ()
writeRegions regionXml = do
    appendFile "Testfile.svg" "<g>"
    appendFile "Testfile.svg" regionXml
    appendFile "Testfile.svg" "</g>"

-- | Writes the `edge` section to the output file.
writeEdges :: String -> IO ()
writeEdges edgeXml = do
    appendFile "Testfile.svg" "<g>"
    appendFile "Testfile.svg" edgeXml 
    appendFile "Testfile.svg" "</g>"

-- | Converts a `Rect` to XML. 
convertRectToXML :: Rect -> String
convertRectToXML rect = 
    if rectFill rect == "none" then "" else
    "<g id=\"" ++ 
    rectId rect ++ 
    "\" class=\"" ++
    (if rectIsHybrid rect then "hybrid" else "node") ++
    "\" in-edges=\"" ++ 
    unwords (rectInEdges rect) ++
    "\" out-edges=\"" ++ 
    unwords (rectOutEdges rect) ++ 
    "\"><rect rx=\"4\" ry=\"4\"  x=\"" ++ 
    show (fromRational $ xPos rect) ++
    "\" y=\"" ++
    show (fromRational $ yPos rect) ++
    "\" width=\"" ++
    show (fromRational $ width rect) ++
    "\" height=\"" ++
    show (fromRational $ height rect) ++
    "\" style=\"fill:" ++
    rectFill rect ++
    ";stroke:#000000" ++ 
    ";fill-opacity:" ++ 
    rectFillOpacity rect ++ 
    ";\"/>" ++ 
    unwords (map convertTextToXML (rectText rect)) ++
    "</g>"

-- | Converts a `Text` to XML.
convertTextToXML :: Text -> String
convertTextToXML text = 
    "<text xml:space=\"preserve\" x=\"" ++ 
    show (fromRational $ textXPos text) ++
    "\" y=\"" ++
    show (fromRational $ textYPos text) ++
    "\" style=\"font-size:" ++
    textFontSize text ++
    ";font-weight:" ++ 
    textFontWeight text ++ 
    ";font-family:" ++
    textFontFamily text ++
    "\">" ++
    textText text ++
    "</text>"

-- | Converts a `Path` to XML.
convertPathToXML :: Path -> String
convertPathToXML path = 
    "<path id=\"" ++ 
    pathId path ++ 
    "\" class=\"path\" style=\"" ++
    "fill:" ++
    pathFill path ++ 
    ";fill-opacity:" ++ 
    pathFillOpacity path ++ 
    ";\" d=\"M " ++
    buildPathString (points path) ++
    "\" marker-end=\"url(#arrow)\" " ++
    "source-node=\"" ++ 
    source path ++ 
    "\" target-node=\"" ++ 
    target path ++ 
    "\"/>"

-- | Converts a `Path` to XML.
convertRegionToXML :: Path -> String
convertRegionToXML path = 
    "<path id=\"region" ++ 
    pathId path ++ 
    "\" class=\"region\" style=\"" ++
    "fill:" ++
    pathFill path ++ 
    ";fill-opacity:" ++ 
    pathFillOpacity path ++ 
    ";\" d=\"M " ++
    buildPathString (points path) ++
    "\"/>"

-- | Converts an `Ellipse` to XML.
convertEllipseToXML :: Ellipse -> String
convertEllipseToXML ellipse = 
    "<g id=\"" ++ 
    ellipseId ellipse ++ 
    "\" class=\"bool\" in-edges=\"" ++
    unwords (ellipseInEdges ellipse) ++
    "\" out-edges=\"" ++ 
    unwords (ellipseOutEdges ellipse) ++ 
    "\">" ++
    "<ellipse cx=\"" ++ 
    show (fromRational $ ellipseXPos ellipse) ++
    "\" cy=\"" ++
    show (fromRational $ ellipseYPos ellipse) ++
    "\" rx=\"" ++ 
    show (fromRational $ ellipseRx ellipse) ++
    "\" ry=\"" ++
    show (fromRational $ ellipseRy ellipse) ++
    "\" style=\"stroke:#000000;fill:none\"/>" ++ 
    unwords (map convertTextToXML (ellipseText ellipse)) ++
    "</g>"