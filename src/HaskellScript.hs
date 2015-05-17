{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Either.Combinators
import Data.Hashable
import System.Directory
import System.FilePath.Posix
import System.Process
import System.IO hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile, length, take, drop, FilePath)
import qualified Prelude as P
import Data.Text
import qualified Data.Text.IO as TIO
import Data.UUID
import qualified Text.Parser.Char as PC
import qualified Text.Parser.Combinators as PCO
import Data.Attoparsec.Text hiding (take)

data ScriptingError = ScriptReadError
                    | ScriptParseError String
                    deriving (Eq, Show)

data ScriptDetails = ScriptDetails [Text] Text

main :: IO ()
main = putStrLn "Hello World!"


runScript :: P.FilePath -> IO ()
runScript scriptPath = do
  result <- runEitherT $ runScriptWithValidation scriptPath
  print result


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


parseScript :: Parser ScriptDetails
parseScript =
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

runScriptWithValidation :: P.FilePath -> EitherT ScriptingError IO ()
runScriptWithValidation scriptPath = do
  -- Read file into memory.
  fileContents <- lift $ TIO.readFile scriptPath
  -- If sandbox containing hash is not present:
  homeDirectory <- lift getHomeDirectory
  let fileHash = hashFileContents fileContents
  let scriptDir = homeDirectory </> ".haskellscript" </> fileHash
  let scriptLocation = scriptDir </> "haskellscript.hs"
  scriptExists <- lift $ doesFileExist $ show scriptLocation
  unless scriptExists $ do
    -- Create hashed path directory.
    lift $ createDirectoryIfMissing True scriptDir
    -- Init sandbox in directory.
    (_, _, _, sandboxInitHandle) <- lift $ createProcess (proc "cabal" ["sandbox", "init"]){ cwd = Just scriptDir }
    lift $ waitForProcess sandboxInitHandle
    -- Parse file into dependencies and the remaining code to run.
    let parseResult = parseOnly parseScript fileContents
    scriptDetails <- hoistEither $ mapLeft ScriptParseError parseResult
    -- For each dependency install it into the sandbox.
    return ()
    -- Create a file containing the remaining code.
  -- Use cabal to runhaskell the script created.
  