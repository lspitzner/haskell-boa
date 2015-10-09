module Constraints (getConstraints, checkConstraints) where

import System.Exit (exitFailure)
import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.Char (isAlphaNum)

import Text.Parsec
import Text.Parsec.String

import Types

data Token =
      TName String
    | TOpenBracket
    | TCloseBracket
    | TComma
    | TForbid
    | TPermit
    | TFor
  deriving (Show, Eq)

lexer :: Parser a -> Parser a
lexer p =
 do r <- p
    spaces
    return r

tName :: Parser Token
tName = lexer $ fmap TName $ many1 $ satisfy (liftA2 (||) isAlphaNum (== '.'))

tOpenBracket :: Parser Token
tOpenBracket = lexer $ fmap (const TOpenBracket) $ char '['

tCloseBracket :: Parser Token
tCloseBracket = lexer $ fmap (const TCloseBracket) $ char ']'

tComma :: Parser Token
tComma = lexer $ fmap (const TComma) $ char ','

tForbid :: Parser Token
tForbid = lexer $ fmap (const TForbid) $ string "forbid"

tPermit :: Parser Token
tPermit = lexer $ fmap (const TPermit) $ string "permit"

tFor :: Parser Token
tFor = lexer $ fmap (const TFor) $ string "for"

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
constraintExpParser = many constraintParser

constraintParser :: Parser Constraint
constraintParser =
 do tFor
    spaces
    constrainedModules <- moduleList
    constraintSpecifier <- tPermit <|> tForbid
    constrainingModules <- moduleList
    if constraintSpecifier == TPermit
      then
        return $ Permitted constrainedModules constrainingModules
      else
        return $ Forbidden constrainedModules constrainingModules

moduleList :: Parser [Module]
moduleList =
 do tOpenBracket
    result <- sepBy1 moduleNameParser tComma
    tCloseBracket
    return result

moduleNameParser :: Parser Module
moduleNameParser = fmap getName $ tName
  where
    getName (TName name) = Name name
    getName _ = error "moduleNameParser: This should not happen."

-- Note: Currently really inefficient. At least quadratic. There should be datastructures which are infinitely better than lists.
checkConstraints :: [Constraint] -> [ModuleImport] -> [Violation]
checkConstraints constraints imports = concatMap (flip helper imports) constraints
  where
    helper :: Constraint -> [ModuleImport] -> [Violation]
    helper c ms = catMaybes $ map (isViolatedBy c) ms

isViolatedBy :: Constraint -> ModuleImport -> Maybe Violation
isViolatedBy c@(Permitted importingModules modules) i@(ModuleImport importer importedModule) =
    if importer `elem` importingModules && importedModule `elem` modules
      then Nothing
      else Just $ Violation c i
isViolatedBy c@(Forbidden importingModules modules) i@(ModuleImport importer importedModule) =
    if importer `elem` importingModules && importedModule `elem` modules
      then Just $ Violation c i
      else Nothing
