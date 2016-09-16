module Main (main) where

import Development.Shake
import Development.Shake.FilePath

import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON, decodeFile)
import Distribution.PackageDescription (allBuildInfo, hsSourceDirs, packageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import GHC.Generics (Generic)


main :: IO ()
main = shakeArgs shakeOptions $
    phony "lint" $ do
        StackConfig {..} <- readYaml "stack.yaml"
        cabals <- getDirectoryFiles "" $ map (</> "*.cabal") packages
        dirs <- concat <$> mapM srcDirs cabals
        cmd "stack exec hlint --" dirs


srcDirs :: FilePath -> Action [FilePath]
srcDirs file = need [file] >> liftIO (dirs <$> readPackageDescription silent file) where
    dirs = concatMap hsSourceDirs . allBuildInfo . packageDescription


data StackConfig = StackConfig
    { packages :: [FilePath]
    } deriving (Generic, FromJSON)

readYaml :: (FromJSON a) => FilePath -> Action a
readYaml file = need [file] >> liftIO (unwrap <$> decodeFile file) where
    unwrap = fromMaybe (error $ "readYaml " ++ file)
