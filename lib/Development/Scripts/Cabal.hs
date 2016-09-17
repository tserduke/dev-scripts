module Development.Scripts.Cabal
    ( srcDirs
    ) where

import Distribution.PackageDescription (allBuildInfo, hsSourceDirs)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)


srcDirs :: FilePath -> IO [FilePath]
srcDirs file = dirs <$> readPackageDescription silent file where
    dirs = concatMap hsSourceDirs . allBuildInfo . flattenPackageDescription
