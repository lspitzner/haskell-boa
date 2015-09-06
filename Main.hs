module Main where

import Arguments (parseArguments)
import HaskellImports (Module, ModuleImport, getImports)

main :: IO ()
main =
 do (constraintLocs, moduleLocs) <- parseArguments
    imports <- getImports moduleLocs
    constraints <- getConstraints constraintLocs
    let violations = checkConstraints constraints imports
    outputViolations violations

type ConstraintLocation = FilePath

data Constraint =
      Permitted [Module] [Module]
    | Forbidden [Module] [Module]

data Violation =
    Violation Constraint ModuleImport

getConstraints :: [ConstraintLocation] -> IO [Constraint]
getConstraints locs = undefined

checkConstraints :: [Constraint] -> [ModuleImport] -> [Violation]
checkConstraints constraints graph = undefined

outputViolations :: [Violation] -> IO ()
outputViolations violations = undefined
