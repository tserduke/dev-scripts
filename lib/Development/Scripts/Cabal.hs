module Development.Scripts.Cabal
    ( readCabal
    , packageVersion
    , showVersion
    , srcDirs
    ) where

import Data.Version (Version, showVersion)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (GenericPackageDescription, allBuildInfo,
    hsSourceDirs, package, packageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)


readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription silent


packageVersion :: GenericPackageDescription -> Version
packageVersion = pkgVersion . package . packageDescription

srcDirs :: GenericPackageDescription -> [FilePath]
srcDirs = concatMap hsSourceDirs . allBuildInfo . flattenPackageDescription
