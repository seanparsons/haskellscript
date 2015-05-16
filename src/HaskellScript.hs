module HaskellScript (
  runScript
)where

import System.Directory
import System.FilePath.Posix
import System.IO hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile)


runScript :: FilePath -> IO ()
runScript scriptPath = do
  -- Read file into memory.
  -- If sandbox containing hash is not present:
    -- Create hashed path directory.
    -- Init sandbox in directory.
    -- Parse file into dependencies and the remaining code to run.
    -- For each dependency install it into the sandbox.
    -- Create a file containing the remaining code.
  -- Use cabal to runhaskell the script created.
