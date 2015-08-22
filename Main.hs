module Main where

main :: IO ()
main =
 do (constraintLocs, moduleLocs) <- parseArguments
    imports <- getImports moduleLocs
    constraints <- getConstraints constraintLocs
    let violations = checkConstraints constraints imports
    outputViolations violations

type ConstraintLocation = FilePath
type ModuleLocation = FilePath

data Constraint =
      Permitted [Module] [Module]
    | Forbidden [Module] [Module]

data Violation =
    Violation Constraint ModuleImport

data Module =
      Name String
    | Namespace String Module

data ModuleImport = ModuleImport Module Module

parseArguments :: IO ([ConstraintLocation], [ModuleLocation])
parseArguments = undefined

getImports :: [ModuleLocation] -> IO [ModuleImport]
getImports locs = undefined

getConstraints :: [ConstraintLocation] -> IO [Constraint]
getConstraints locs = undefined

checkConstraints :: [Constraint] -> ImportGraph -> [Violation]
checkConstraints constraints graph = undefined

outputViolations :: [Violation] -> IO ()
outputViolations violations = undefined
