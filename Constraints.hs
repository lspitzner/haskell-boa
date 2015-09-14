module Constraints (getConstraints, checkConstraints) where

import System.Exit (exitFailure)
import Control.Monad (void)

import Text.Parsec
import Text.Parsec.String

import Types

getConstraints :: [ConstraintLocation] -> IO [Constraint]
getConstraints locs = fmap concat $ mapM parseConstraints locs

parseConstraints :: ConstraintLocation -> IO [Constraint]
parseConstraints loc =
 do result <- parseFromFile constraintExpParser loc
    case result of
      Left error ->
         do putStrLn $ show error
            exitFailure
      Right constraints ->
        return constraints

constraintExpParser :: Parser [Constraint]
constraintExpParser = undefined

constraintParser :: Parser Constraint
constraintParser =
 do string "for"
    spaces
    constrainedModules <- moduleNames
    spaces
    constraintSpecifier <- string "permit" <|> string "forbid"
    spaces
    constrainingModules <- moduleNames
    (void endOfLine) <|> eof
    if constraintSpecifier == "permit"
      then
        return $ Permitted constrainedModules constrainingModules
      else
        return $ Forbidden constrainedModules constrainingModules

moduleNames :: Parser [Module]
moduleNames =
 do spaces
    char '['
    spaces
    result <- sepBy1 moduleNameParser (spaces >> char ',' >> spaces)
    spaces
    char ']'
    return result

moduleNameParser :: Parser Module
moduleNameParser = fmap Name $ many1 anyChar 

checkConstraints :: [Constraint] -> [ModuleImport] -> [Violation]
checkConstraints constraints imports = undefined
