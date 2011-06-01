module Inkvizitor.LocFile (
    locFile
  ) where

import Geocode 
  ( Coords(..)
  , Location(..)
  )
  

locFile :: [Location] -> String
locFile locations = header ++ concat (map locEntry locations) ++ footer
  where locEntry :: Location -> String
        locEntry loc =
             "<waypoint>\n" 
          ++ "  <name id=\"" ++ getLocId loc ++ "\">"
          ++ "<![CDATA[" ++ getFullAddress loc ++ "\"]]>"
          ++ "</name>\n"
          ++ "  <coord"
          ++ " lat=\"" ++ (show . getLatitude . getCoords $ loc) ++ "\""
          ++ " lon=\"" ++ (show . getLongitude . getCoords $ loc) ++ "\"/>\n"
          ++ "</waypoint>\n\n"

        footer = "</loc>\n"
        header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
              ++ "<loc src=\"Geocode\" version=\"1.0\">\n\n"

