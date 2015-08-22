module Arguments (parseArguments) where

import System.Environment (getArgs)

parseArguments :: IO ([FilePath], [FilePath])
parseArguments =
 do args <- getArgs
    validatedArgs <- validateArgs args
    files <- expandDirectories validatedArgs
    return $ splitBy isConstraint (filter (liftA2 (||) isHaskell isConstraint) files)

expandDirectories :: [FilePath] -> IO [FilePath]
expandDirectories paths =
 do let (dirs, files) = splitBy isDirectory paths
    deeperPaths <- concatMap getContents dirs
    deeperfiles <- expandDirectories deeperPaths
    return $ files ++ deeperFiles 

isConstraint :: FilePath -> Bool
isConstraint = undefined -- ends with '.constraint'

isHaskell :: FilePath -> Bool
isHaskell = undefined -- ends with '.hs' or '.lhs' (not sure whether this is sufficient)
