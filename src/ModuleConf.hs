module ModuleConf where

import Conf

import Distribution.PackageDescription (Library,ConfVar,CondTree,condLibrary)
import Distribution.Package (Dependency)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)

import Control.Monad.IO.Class (liftIO)

data ModuleFlag = ModuleFlag

packageConfiguration :: FilePath -> ConfT [String] ModuleFlag IO ([String],[FilePath])
packageConfiguration filepath = do
    genericPackageDescription <- liftIO (readPackageDescription silent filepath)
    case condLibrary genericPackageDescription of
        Nothing -> liftIO (putStrLn "No Library") >> return ([],[])
        Just l  -> libraryConfig l

libraryConfig :: CondTree ConfVar [Dependency] Library -> ConfT [String] ModuleFlag m ([String],[FilePath])
libraryConfig = undefined

