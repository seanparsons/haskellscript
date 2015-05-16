{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Monad
import Control.Monad.Trans
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
import qualified Text.Parser.Char as PC
import qualified Text.Parser.Combinators as PCO
import Data.Attoparsec.Text hiding (take)

data ScriptingError = ScriptReadError deriving (Eq, Show)

data ScriptDetails = ScriptDetails [Text] Text

main :: IO ()
main = putStrLn "Hello World!"


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
      fourth          = hash $ drop (chunkSize * 3) contents
  in  show $ fromWords (fromIntegral first) (fromIntegral second) (fromIntegral third) (fromIntegral fourth)


parseScript :: Text -> Parser ScriptDetails
parseScript scriptContents =
  let endOfLineChars    = ['\r', '\n']
      isEOLChar         = PC.oneOf endOfLineChars
      notEOLChar        = PC.noneOf endOfLineChars
      emptyLine         = PC.spaces >> PCO.many isEOLChar
      packageLine       = fmap pack $ PCO.between (PC.text "--#" >> PC.spaces) emptyLine (PCO.many notEOLChar)
      shebangLine       = PC.text "#!" >> PCO.many notEOLChar >> PCO.many isEOLChar
      packageLineSkip   = PCO.many emptyLine >> packageLine
      packageLines      = PCO.many packageLineSkip
      remainder         = fmap pack (PCO.many emptyLine >> PCO.many PC.anyChar)
  in  do
        shebangLine
        packageDetails <- packageLines
        scriptContent  <- remainder
        return $ ScriptDetails packageDetails scriptContent

runScriptWithValidation :: P.FilePath -> ValidationT ScriptingError IO ()
runScriptWithValidation scriptPath = do
  -- Read file into memory.
  fileContents <- lift $ TIO.readFile scriptPath
  -- If sandbox containing hash is not present:
  homeDirectory <- lift getHomeDirectory
  let fileHash = hashFileContents fileContents
  let scriptDir = homeDirectory </> ".haskellscript" </> fileHash
  let scriptLocation = scriptDir </> "haskellscript.hs"
  scriptExists <- lift $ doesFileExist $ show scriptLocation
  lift $ unless scriptExists $ shelly $ do
    -- Create hashed path directory.
    mkdir_p scriptDir
    -- Init sandbox in directory.
    cd scriptDir
    run_ "cabal" ["sandbox", "init"]
    -- Parse file into dependencies and the remaining code to run.

    -- For each dependency install it into the sandbox.
    -- Create a file containing the remaining code.
  -- Use cabal to runhaskell the script created.
  