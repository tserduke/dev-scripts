{-# LANGUAGE RecordWildCards #-}

module Development.Scripts
    ( lint
    , build
    , checkChangelog
    , publish
    , rules
    , runAction
    ) where

import Development.Shake
import Development.Shake.FilePath

import Development.Scripts.Cabal
import Development.Scripts.Markdown
import Development.Scripts.Shake
import Development.Scripts.Stack

import Data.Maybe (fromMaybe)
import Data.Sequence (index)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.Text as T


lint, build, checkChangelog, publish :: IO ()
lint           = execute "lint"
build          = execute "build"
checkChangelog = execute "check-changelog"
publish        = execute "publish"

execute :: String -> IO ()
execute name = shake shakeOptions $ want [name] >> rules

runAction :: Action a -> IO ()
runAction = shake shakeOptions . action


rules :: Rules ()
rules = do
    phony "publish" $ do
        Stdout files <- cmd "hg status"
        assertEqual "Uncommitted Changes" "" files
        need ["lint", "build", "check-changelog"]
        () <- cmd (Traced "clean") "hg clean src"
        () <- cmd (Traced "sdist") "stack sdist ."
        () <- cmd (Traced "upload") "stack upload ."
        version <- packageVersion <$> getPackage
        () <- cmd (Traced "tag") "hg tag" ("v" ++ version)
        need ["hackage-docs"]
        putNormal "Published"

    phony "lint" $ do
        StackConfig {..} <- withNeed readYaml "stack.yaml"
        let patterns = map (</> "*.cabal") $ fromMaybe ["."] packages
        cabalFiles <- getDirectoryFiles "" patterns
        cabals <- mapM (withNeed readCabal) cabalFiles
        let dirs = concatMap srcDirs cabals
        cmd (Traced "hlint") "stack exec hlint --" dirs

    phony "build" $ do
        () <- cmd (Traced "reset") "stack clean"
        () <- cmd (Traced "build") "stack build --test --bench"
            "--ghc-options \"-Werror\" --no-run-benchmarks"
        name <- packageName <$> getPackage
        () <- cmd (Traced "haddock") "stack build --haddock"
            "--ghc-options \"-Werror\"" name
        putNormal "Build Successful"

    phony "check-changelog" $ do
        version <- packageVersion <$> getPackage
        changelog <- withNeed readMarkdown "changelog.md"
        let (Header 2 header) = index changelog 1
        let [version', date] = map T.unpack $ T.words $ inlinesText header
        assertEqual "Changelog Version" version version'
        time <- liftIO getCurrentTime
        let today = formatTime defaultTimeLocale "(%Y-%m-%d)" time
        assertEqual "Changelog Date" today date
        putNormal "Changelog OK"

    phony "hackage-docs" $ withTempDir $ \temp -> do
        pkgId <- packageId <$> getPackage
        putNormal pkgId
        putNormal "Docs Uploaded"

getPackage :: Action GenericPackageDescription
getPackage = do
    [file] <- getDirectoryFiles "" ["*.cabal"]
    withNeed readCabal file
