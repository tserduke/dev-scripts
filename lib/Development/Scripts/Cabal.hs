module Development.Scripts.Cabal
    ( GenericPackageDescription
    , readCabal
    , packageId
    , packageName
    , packageVersion
    , srcDirs
    ) where

import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Version (showVersion)
import Distribution.Package (PackageIdentifier, pkgName, pkgVersion, unPackageName)
import Distribution.PackageDescription hiding (allBuildInfo)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)


readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription silent


packageId, packageName, packageVersion :: GenericPackageDescription -> String
packageId x = packageName x ++ "-" ++ packageVersion x

packageName = unPackageName . pkgName . getPackage
packageVersion = showVersion . pkgVersion . getPackage

getPackage :: GenericPackageDescription -> PackageIdentifier
getPackage = package . packageDescription


srcDirs :: GenericPackageDescription -> [FilePath]
srcDirs = nub . concatMap hsSourceDirs . allBuildInfo . flattenPackageDescription

allBuildInfo :: PackageDescription -> [BuildInfo]
allBuildInfo pkg = concat
    [ map libBuildInfo       $ maybeToList $ library pkg
    , map buildInfo          $ executables           pkg
    , map testBuildInfo      $ testSuites            pkg
    , map benchmarkBuildInfo $ benchmarks            pkg
    ]
