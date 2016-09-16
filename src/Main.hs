module Main
    ( main
    , lint
    ) where

import Development.Shake
import Development.Shake.FilePath

import Control.Exception (SomeException, handle)
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON, decodeFile)
import Distribution.PackageDescription (allBuildInfo, hsSourceDirs)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import GHC.Generics (Generic)


main, lint :: IO ()
main = shakeArgs shakeOptions $
    phony "lint" doLint

lint = execute doLint

doLint :: Action ()
doLint = do
    StackConfig {..} <- withNeed readYaml "stack.yaml"
    let patterns = map (</> "*.cabal") $ fromMaybe ["."] packages
    cabals <- getDirectoryFiles "" patterns
    dirs <- concat <$> mapM (withNeed srcDirs) cabals
    cmd "stack exec hlint --" dirs


execute :: Action a -> IO ()
execute = handle onError . shake shakeOptions . action where
    onError = const $ putStrLn "Fail" :: SomeException -> IO ()


srcDirs :: FilePath -> IO [FilePath]
srcDirs file = dirs <$> readPackageDescription silent file where
    dirs = concatMap hsSourceDirs . allBuildInfo . flattenPackageDescription


data StackConfig = StackConfig
    { packages :: Maybe [FilePath]
    } deriving (Generic, FromJSON)

readYaml :: (FromJSON a) => FilePath -> IO a
readYaml file = unwrap <$> decodeFile file where
    unwrap = fromMaybe (error $ "readYaml " ++ file)


-- TODO: push upstream
withNeed :: (FilePath -> IO a) -> FilePath -> Action a
withNeed func file = need [file] >> liftIO (func file)
