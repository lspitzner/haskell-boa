module Constraints (getConstraints, checkConstraints) where

import System.Exit (exitFailure)
import Control.Applicative (liftA2)
import Control.Monad (void, guard)
import Data.Maybe (mapMaybe)
import Data.Char (isAlphaNum)
import Data.Functor (($>), (<$), (<$>))

import Text.Parsec
import Text.Parsec.String

import Types

lexer :: Parser a -> Parser a
lexer = (<* spaces)

tName :: Parser String
tName = lexer $ many1 $ satisfy (liftA2 (||) isAlphaNum (== '.'))

tOpenBracket :: Parser ()
tOpenBracket = lexer $ void $ char '['

tCloseBracket :: Parser ()
tCloseBracket = lexer $ void $ char ']'

tComma :: Parser ()
tComma = lexer $ void $ char ','

tForbid :: Parser ()
tForbid = lexer $ void $ string "forbid"

tPermit :: Parser ()
tPermit = lexer $ void $ string "permit"

tFor :: Parser ()
tFor = lexer $ void $ string "for"

getConstraints :: [ConstraintLocation] -> IO [Constraint]
getConstraints locs = concat <$> mapM parseConstraints locs

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
constraintExpParser = many constraintParser

constraintParser :: Parser Constraint
constraintParser =
 do tFor
    spaces
    constrainedModules <- moduleList
    constraintSpecifier <- Permitted <$ tPermit
                       <|> Forbidden <$ tForbid
    constrainingModules <- moduleList
    return $ constraintSpecifier constrainedModules constrainingModules

moduleList :: Parser [Module]
moduleList = between tOpenBracket tCloseBracket
           $ sepBy1 moduleNameParser tComma

moduleNameParser :: Parser Module
moduleNameParser = Name <$> tName

-- Note: Currently really inefficient. At least quadratic. There should be datastructures which are infinitely better than lists.
checkConstraints :: [Constraint] -> [ModuleImport] -> [Violation]
checkConstraints constraints imports = concatMap (flip helper imports) constraints
  where
    helper :: Constraint -> [ModuleImport] -> [Violation]
    helper c ms = mapMaybe (isViolatedBy c) ms

isViolatedBy :: Constraint -> ModuleImport -> Maybe Violation
isViolatedBy c@(Permitted importingModules modules) i@(ModuleImport importer importedModule) = do
  guard $ not $ importer `elem` importingModules && importedModule `elem` modules
  return $ Violation c i
isViolatedBy c@(Forbidden importingModules modules) i@(ModuleImport importer importedModule) = do
  guard $ importer `elem` importingModules && importedModule `elem` modules
  return $ Violation c i
