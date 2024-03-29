module Main where

import Arguments (parseArguments)
import HaskellImports (getImports)
import Constraints (getConstraints, checkConstraints)
import Types

main :: IO ()
main =
 do (_, constraintLocs, moduleLocs) <- parseArguments
    -- the command is discarded because currently there is only one of them
    -- more will (hopefully) come in the future
    putStrLn (show constraintLocs)
    putStrLn (show moduleLocs)
    imports <- getImports moduleLocs
    constraints <- getConstraints constraintLocs
    let violations = checkConstraints constraints imports
    outputViolations violations

outputViolations :: [Violation] -> IO ()
outputViolations vs = putStrLn $ unlines $ map showViolation vs

showViolation :: Violation -> String
showViolation (Violation constr (ModuleImport (Name importer) (Name imported))) =
    case constr of
      Permitted _ _ -> "Non-permitted import of " ++ imported ++ " in " ++ importer
      Forbidden _ _ -> "Forbidden import of " ++ imported ++ " in " ++ importer
