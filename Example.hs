#!/usr/bin/env haskellscript
--#aeson
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.ByteString.Lazy hiding (putStrLn, unpack)
import Data.Text
import Data.Text.Encoding
main = putStrLn $ unpack $ decodeUtf8 $ toStrict $ encode $ object ["Test" .= True, "Example" .= True]