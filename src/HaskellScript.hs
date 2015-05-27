{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Crypto.Hash
import Data.Either.Combinators
import Data.Monoid
import Data.Traversable
import qualified Data.List as L
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix
import System.Process
import System.IO hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile, length, take, drop, FilePath, lines, unlines)
import qualified Prelude as P
import Data.Text hiding (filter)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

data ScriptingError = ScriptReadError
                    | ScriptParseError String
                    deriving (Eq, Show)

data ScriptDetails = ScriptDetails 
                   { scriptDependencies :: [Text]
                   , scriptText :: Text
                   }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [script]  -> runScript script
    _         -> putStrLn "Command line arguments are incorrect, please pass a script file."


runScript :: P.FilePath -> IO ()
runScript scriptPath = do
  result <- runEitherT $ runScriptWithValidation scriptPath
  case result of
    (Right _)     -> return ()
    (Left error)  -> putStrLn "Error handling script: " >> print error

isDependencyLine :: Text -> Bool
isDependencyLine line = isPrefixOf "--#" line


parseScript :: Text -> Either ScriptingError ScriptDetails
parseScript script = do
  let scriptLines = lines script
  afterHashbang <- case scriptLines of
    first : rest  -> if isPrefixOf "#!" first then Right rest else Left $ ScriptParseError "No shebang at start of script."
    _             -> Left $ ScriptParseError "Script is empty."
  let (dependenciesLines, remainder) = L.span (\line -> isDependencyLine line || (length $ strip line) == 0) afterHashbang
  let dependencies = L.sort $ fmap (strip . drop 3) $ filter isDependencyLine dependenciesLines
  return $ ScriptDetails dependencies (unlines remainder)


getDependenciesHash :: ScriptDetails -> String
getDependenciesHash details = 
  let tohashBytes = TE.encodeUtf8 $ intercalate "," $ scriptDependencies details
  in  show (hash tohashBytes :: Digest SHA3_512)


getContentHash :: ScriptDetails -> String
getContentHash details =
  let tohashBytes = TE.encodeUtf8 $ scriptText details
  in  show (hash tohashBytes :: Digest SHA3_512)


runInWorkingDir :: FilePath -> FilePath -> [String] -> EitherT ScriptingError IO ()
runInWorkingDir workingDir toRun params = lift $ do
  (_, _, _, sandboxInitHandle) <- createProcess (proc toRun params){ cwd = Just workingDir }
  waitForProcess sandboxInitHandle
  return ()


runScriptWithValidation :: P.FilePath -> EitherT ScriptingError IO ()
runScriptWithValidation scriptPath = do
  -- Read file into memory.
  fileContents <- lift $ TIO.readFile scriptPath
  -- Parse file into dependencies and the remaining code to run.
  scriptDetails <- hoistEither $ parseScript fileContents
  -- If sandbox containing hash is not present:
  homeDirectory <- lift getHomeDirectory
  let dependenciesHash = getDependenciesHash scriptDetails
  let scriptHash = getContentHash scriptDetails
  let scriptDir = homeDirectory </> ".haskellscript" </> dependenciesHash
  let dependenciesMarker = scriptDir </> ".dependencieswritten"
  dependenciesMarkerExists <- lift $ doesFileExist dependenciesMarker
  unless dependenciesMarkerExists $ do
    -- Remove the directory first.
    scriptDirExists <- lift $ doesDirectoryExist scriptDir
    lift $ when scriptDirExists $ removeDirectoryRecursive scriptDir
    -- Create hashed path directory.
    lift $ createDirectoryIfMissing True scriptDir
    -- Init sandbox in directory.
    runInWorkingDir scriptDir "cabal" ["sandbox", "init"]
    -- For each dependency install it into the sandbox.
    traverse (\dep -> runInWorkingDir scriptDir "cabal" ["install", (unpack dep)]) (scriptDependencies scriptDetails)
    -- Write file to confirm dependencies have been written.
    lift $ TIO.writeFile dependenciesMarker mempty
    return ()
  let scriptLocation = scriptDir </> scriptHash <.> "hs"
  -- Create a file containing the code.
  lift $ TIO.writeFile scriptLocation (scriptText scriptDetails)
  -- Use cabal to runhaskell the script created.
  runInWorkingDir scriptDir "cabal" ["exec", "runghc", scriptLocation]
