module Main where

import Arguments (parseArguments)
import HaskellImports (Module, ModuleImport, getImports)
import Types

main :: IO ()
main =
 do (constraintLocs, moduleLocs) <- parseArguments
    imports <- getImports moduleLocs
    constraints <- getConstraints constraintLocs
    let violations = checkConstraints constraints imports
    outputViolations violations

getConstraints :: [ConstraintLocation] -> IO [Constraint]
getConstraints locs = undefined

checkConstraints :: [Constraint] -> [ModuleImport] -> [Violation]
checkConstraints constraints graph = undefined

outputViolations :: [Violation] -> IO ()
outputViolations violations = undefined
