module Development.Scripts.Cabal
    ( GenericPackageDescription
    , readCabal
    , packageId
    , packageName
    , packageVersion
    , srcDirs
    ) where

import Data.Version (showVersion)
import Distribution.Package (PackageIdentifier, pkgName, pkgVersion, unPackageName)
import Distribution.PackageDescription (GenericPackageDescription, allBuildInfo,
    hsSourceDirs, package, packageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)


readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription silent


packageId, packageName, packageVersion :: GenericPackageDescription -> String
packageId x = packageName x ++ "-" ++ packageVersion x

packageName = unPackageName . pkgName . toPackage
packageVersion = showVersion . pkgVersion . toPackage

toPackage :: GenericPackageDescription -> PackageIdentifier
toPackage = package . packageDescription


srcDirs :: GenericPackageDescription -> [FilePath]
srcDirs = concatMap hsSourceDirs . allBuildInfo . flattenPackageDescription
