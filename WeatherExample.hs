#!/usr/bin/env haskellscript
--#aeson-0.8.1.0
--#http-conduit-2.1.5

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy hiding (putStr, putStrLn, unpack, drop)
import Data.Char (toLower)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Text.Encoding
import Network.HTTP.Conduit

data IPLookupResult = IPLookupResult
                    { city :: String
                    , longitude :: Double
                    , latitude :: Double
                    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''IPLookupResult)

data Weather = Weather
             { description :: String
             } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Weather)

data WeatherMain = WeatherMain
                 { temp :: Double
                 } deriving (Eq, Show)

$(deriveJSON defaultOptions ''WeatherMain)

data WeatherLookupResult = WeatherLookupResult
                         { resultWeather :: [Weather]
                         , resultMain :: WeatherMain
                         } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . drop 6} ''WeatherLookupResult)

lookupIP :: IO IPLookupResult
lookupIP = do
  ipLookupBS <- simpleHttp "https://freegeoip.net/json/"
  maybe (fail "Couldn't decode IP lookup response.") return $ decode ipLookupBS

lookupWeather :: IPLookupResult -> IO WeatherLookupResult
lookupWeather (IPLookupResult ipCity ipLongtitude ipLatitude) = do
  weatherLookupBS <- simpleHttp ("http://api.openweathermap.org/data/2.5/weather?lat=" ++ (show ipLatitude) ++ "&lon=" ++ (show ipLongtitude))
  maybe (fail "Couldn't decode weather lookup response.") return $ decode weatherLookupBS

printWeatherDetails :: IPLookupResult -> WeatherLookupResult -> IO ()
printWeatherDetails (IPLookupResult location _ _) (WeatherLookupResult [(Weather weatherDescription)] (WeatherMain weatherTemp)) = do
  putStr "The weather in "
  putStr location
  putStr " is: "
  putStrLn weatherDescription
  putStr "With the temperature currently at: "
  print $ round (weatherTemp - 273.16)
printWeatherDetails _ _ = putStrLn "Not a full set of weather info."

main = do
  ipResult <- lookupIP
  weatherResult <- lookupWeather ipResult
  printWeatherDetails ipResult weatherResult
