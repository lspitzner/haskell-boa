module Types where

data Command = Check

type ConstraintLocation = FilePath

data Constraint =
      Permitted [Module] [Module] -- TODO: rename to Whitelist
    | Forbidden [Module] [Module] --                 Blacklist
                                  -- because "Permitted" does not sound like
                                  -- it would ever lead to a constraint
                                  -- violation.

data Violation =
    Violation Constraint ModuleImport

type ModuleLocation = FilePath

data ModuleImport = ModuleImport Module Module

newtype Module = Name String
    deriving (Eq)
