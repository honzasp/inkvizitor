module Inkvizitor.Geocode (
    Coords(..)
  , Query(..)
  , Location(..)
  , geocode
  ) where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Monad
import Network.HTTP
import Network.URI
import Text.JSON 
  ( decode
  , fromJSObject
  , fromJSString
  , resultToEither
  , JSValue(..)
  )

data Coords = Coords {
    getLatitude :: Double
  , getLongitude :: Double
  } deriving (Eq, Show)

-- | Query saved in input.txt (to be geocoded)
data Query = Query {
    getAddress :: String -- | Address (e.g. "Silicon Valley")
  , getQueryId :: String -- | An identifier (e.g. "my work")
  } deriving (Eq, Show)

-- | Result of geocoding (exact location)
data Location = Location {
    getCoords :: Coords
  , getLocId :: String
  , getFullAddress :: String
  } deriving (Eq, Show)

-- | Returns an IO action that returns either error message or list of found
-- locations.
geocode :: Query -> IO (Either String [Location])
geocode query = do
  let uri = "http://maps.googleapis.com/maps/api/geocode/json?" ++ params
      params = "address=" ++ escape (getAddress query) ++ "&sensor=false"
      escape = escapeURIString isAllowedInURI . encodeString
  -- get HTTP result
  result <- simpleHTTP (getRequest uri)
  case result of
    Left err -> return $ Left ("HTTP error: " ++ show err)
    Right response -> do
      -- parse JSON
      let body = rspBody response
          jsResult = Text.JSON.decode body
      return (resultToEither jsResult >>= processJSON query)

-- helpers
fjo = fromJSObject
fjs = fromJSString

-- | Processes parsed JSON response
processJSON :: Query -> JSValue -> Either String [Location]
processJSON query (JSObject topObj) =
  case fjs status of
    "OK" -> Right $ map (processResult query) results
    "ZERO_RESULTS" -> Right []
    err -> Left $ "Geocoding API returned error: " ++ err
  where toplevel = fjo topObj
        -- fields of toplevel JSON object
        Just (JSString status) = lookup "status" toplevel
        Just (JSArray results) = lookup "results" toplevel

-- | Processes one result
processResult :: Query -> JSValue -> Location
processResult query (JSObject result) =
  Location {
      getCoords = coords
    , getLocId = getQueryId query
    , getFullAddress = decodeString . fjs $ fullAddr
  }
  where coords = Coords (fromRational lat) (fromRational lng)
        Just (JSRational _ lat) = lookup "lat" (fjo location)
        Just (JSRational _ lng) = lookup "lng" (fjo location)
        Just (JSObject location) = lookup "location" (fjo geometry)
        Just (JSObject geometry) = lookup "geometry" (fjo result)
        Just (JSString fullAddr) = lookup "formatted_address" (fjo result)
