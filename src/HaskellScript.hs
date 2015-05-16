{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module HaskellScript (
  runScript
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Hashable
import System.Directory
import Shelly
import System.IO hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile, length, take, drop, FilePath)
import qualified Prelude as P
import Data.Text
import qualified Data.Text.IO as TIO
import Data.Validation
import Data.UUID

data ScriptingError = ScriptReadError deriving (Eq, Show)

runScript :: P.FilePath -> IO ()
runScript scriptPath = do
  validation <- runValidationT $ runScriptWithValidation scriptPath
  print validation


hashFileContents :: Text -> String
hashFileContents contents = 
  let contentSize     = length contents
      chunkSize       = contentSize `div` 4
      chunk n         = hash $ take chunkSize $ drop (chunkSize * n) contents
      first           = chunk 0
      second          = chunk 1
      third           = chunk 2
      fourth          = chunk 3
  in  show $ fromWords (fromIntegral first) (fromIntegral second) (fromIntegral third) (fromIntegral fourth)


runScriptWithValidation :: P.FilePath -> ValidationT ScriptingError IO ()
runScriptWithValidation scriptPath = do
  -- Read file into memory.
  fileContents <- liftIO $ TIO.readFile scriptPath
  -- If sandbox containing hash is not present:
  homeDirectory <- liftIO getHomeDirectory
  let fileHash = hashFileContents fileContents
  let scriptDir = homeDirectory </> ".haskellscript" </> fileHash
  let scriptLocation = scriptDir </> "haskellscript.hs"
  scriptExists <- liftIO $ doesFileExist $ show scriptLocation
  liftIO $ unless scriptExists $ shelly $ do
    -- Create hashed path directory.
    mkdir_p scriptDir
    -- Init sandbox in directory.
    cd scriptDir
    run_ "cabal" ["sandbox", "init"]
    -- Parse file into dependencies and the remaining code to run.
    -- For each dependency install it into the sandbox.
    -- Create a file containing the remaining code.
  -- Use cabal to runhaskell the script created.
  