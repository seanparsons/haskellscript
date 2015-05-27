#!/usr/bin/env haskellscript
--#aeson-0.8.1.0
--#http-conduit-2.1.5
--#lens-aeson-1.0.0.4

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Data.Text (unpack)
import Network.HTTP.Conduit

maybeFail f m = maybe (fail f) return m

main = do
  ipLookupBS        <- simpleHttp "https://freegeoip.net/json/"
  city              <- maybeFail "Can't parse city." (ipLookupBS ^? key "city" . _String)
  latitude          <- maybeFail "Can't parse latitude." (ipLookupBS ^? key "latitude" . _Double)
  longitude         <- maybeFail "Can't parse longitude." (ipLookupBS ^? key "longitude" . _Double)
  weatherLookupBS   <- simpleHttp ("http://api.openweathermap.org/data/2.5/weather?lat=" ++ (show latitude) ++ "&lon=" ++ (show longitude))
  description       <- maybeFail "Can't parse description." (weatherLookupBS ^? key "weather" . nth 0 . key "description" . _String)
  temperature       <- maybeFail "Can't parse temperature." $ (weatherLookupBS ^? key "main" . key "temp" . _Double)
  putStrLn ("The weather in " ++ (unpack city) ++ " is " ++ (unpack description) ++ ".")
  putStrLn ("With the temperature currently at " ++ (show $ round (temperature - 273.16)) ++ " degrees.")
