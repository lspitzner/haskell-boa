module Types where

data Command = Check

type ConstraintLocation = FilePath

data Constraint =
      Permitted [Module] [Module]
    | Forbidden [Module] [Module]

data Violation =
    Violation Constraint ModuleImport

type ModuleLocation = FilePath

data ModuleImport = ModuleImport Module Module

newtype Module = Name String
    deriving (Eq)
