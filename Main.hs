module Main where

import Arguments (parseArguments)
import HaskellImports (getImports)
import Constraints (getConstraints, checkConstraints)
import Types

main :: IO ()
main =
 do (constraintLocs, moduleLocs) <- parseArguments
    imports <- getImports moduleLocs
    constraints <- getConstraints constraintLocs
    let violations = checkConstraints constraints imports
    outputViolations violations

outputViolations :: [Violation] -> IO ()
outputViolations violations = undefined
