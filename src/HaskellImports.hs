module HaskellImports (Module, ModuleImport, getImports) where

import Types
import Language.Haskell.Exts (parseFile, fromParseResult)
import qualified Language.Haskell.Exts as L


import System.Exit (exitFailure)


getImports :: [ModuleLocation] -> IO [ModuleImport]
getImports locs =
 do parseResults <- mapM parseFile locs
    let modules = map fromParseResult parseResults -- throws error if parse fails
    return $ concatMap moduleToImports modules

moduleToImports :: L.Module -> [ModuleImport]
moduleToImports (L.Module _ name _ _ _ imports _) =
 let
    nameToModule (L.ModuleName s) = Name s
    importedNames = map (nameToModule . L.importModule) imports
    importingModule = nameToModule name
 in
    map (\name -> ModuleImport importingModule name) importedNames
