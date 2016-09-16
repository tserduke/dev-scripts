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
        StackConfig {..} <- withNeed readYaml "stack.yaml"
        let patterns = map (</> "*.cabal") $ fromMaybe ["."] packages
        cabals <- getDirectoryFiles "" patterns
        dirs <- concat <$> mapM (withNeed srcDirs) cabals
        putNormal $ show dirs
        cmd "stack exec hlint --" dirs


srcDirs :: FilePath -> IO [FilePath]
srcDirs file = dirs <$> readPackageDescription silent file where
    dirs = concatMap hsSourceDirs . allBuildInfo . packageDescription


data StackConfig = StackConfig
    { packages :: Maybe [FilePath]
    } deriving (Generic, FromJSON)

readYaml :: (FromJSON a) => FilePath -> IO a
readYaml file = unwrap <$> decodeFile file where
    unwrap = fromMaybe (error $ "readYaml " ++ file)


withNeed :: (FilePath -> IO a) -> FilePath -> Action a
withNeed func file = need [file] >> liftIO (func file)
