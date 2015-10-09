module Arguments (parseArguments) where

import Types (Command(Check))

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, isValid, (</>))

import Control.Applicative (liftA2)
import Data.List (partition, isSuffixOf)

parseArguments :: IO (Command, [FilePath], [FilePath])
parseArguments =
 do args <- getArgs
    if null args
      then
         do putStrLn "Missing Arguments."
            exitFailure
      else
         do c <- fmap head getArgs
            command <- checkCommand c
            (p1, p2) <- parsePathArguments
            return (command, p1, p2)

checkCommand :: String -> IO Command
checkCommand "check" = return Check
checkCommand c = putStrLn ("Invalid Command: " ++ c) >> exitFailure

parsePathArguments :: IO ([FilePath], [FilePath])
parsePathArguments =
 do pathargs <- fmap (drop 1) getArgs
    validatedArgs <- validateArgs pathargs
    files <- expandDirectories validatedArgs
    return $ partition isConstraint (filter (liftA2 (||) isHaskell isConstraint) files)

validateArgs :: [String] -> IO [String]
validateArgs args =
 do paths <- validatePaths args
    checkExistence paths

validatePaths :: [String] -> IO [FilePath]
validatePaths paths =
 do let (validPaths, nonValidPaths) = partition isValid paths
    if (not . null) nonValidPaths
      then
         do mapM (\p -> putStrLn ("Invalid Path: " ++ p)) nonValidPaths
            exitFailure
      else
        return validPaths

checkExistence :: [FilePath] -> IO [FilePath]
checkExistence paths =
 do checks <- mapM doesDirectoryOrFileExist paths
    let checkedPaths = zip paths checks
    let failedPaths = map fst $ filter (not . snd) checkedPaths
    if (not . null) failedPaths
      then
         do mapM (\p -> putStrLn ("File or Directory does not exist: " ++ p)) failedPaths
            exitFailure
      else return $ map fst checkedPaths

doesDirectoryOrFileExist :: FilePath -> IO Bool
doesDirectoryOrFileExist = liftA2 (liftA2 (||)) doesDirectoryExist doesFileExist

expandDirectories :: [FilePath] -> IO [FilePath]
expandDirectories [] = return []
expandDirectories paths =
 do let nontrivialPaths = filter (liftA2 (&&) (not . isSuffixOf ".") (not . isSuffixOf "..")) paths
    (dirs, files) <- partitionM doesDirectoryExist nontrivialPaths
    deeperPaths <- fmap concat $ mapM (\dir -> fmap (map (dir </>)) $ getDirectoryContents dir) dirs
    deeperFiles <- expandDirectories deeperPaths
    return $ files ++ deeperFiles

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a]) 
partitionM pred list =
    case list of
      (a : as) ->
       do b <- pred a
          (accepted, declined) <- partitionM pred as
          if b
            then return (a : accepted, declined)
            else return (accepted, a : declined)
      [] ->
        return ([], [])

-- ends with '.constraint'
isConstraint :: FilePath -> Bool
isConstraint = (== ".constraint") . takeExtension

-- ends with '.hs' or '.lhs' (not sure whether this is sufficient)
isHaskell :: FilePath -> Bool
isHaskell = (liftA2 (||) (== ".hs") (== ".lhs")) . takeExtension
