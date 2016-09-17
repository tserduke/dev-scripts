{-# LANGUAGE RecordWildCards #-}

module Development.Scripts
    ( lint
    , build
    , checkChangelog
    , publish
    , rules
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


rules :: Rules ()
rules = do
    phony "publish" $ do
        need ["lint", "build", "check-changelog"]
        Stdout files <- cmd "hg st"
        assertEqual "Uncommited Files" "" files
        () <- cmd (Traced "sdist") "stack sdist"
        () <- cmd (Traced "upload") "stack upload ."
        version <- getVersion
        () <- cmd "hg tag" ("v" ++ version)
        putNormal "Published"

    phony "lint" $ do
        StackConfig {..} <- withNeed readYaml "stack.yaml"
        let patterns = map (</> "*.cabal") $ fromMaybe ["."] packages
        cabalFiles <- getDirectoryFiles "" patterns
        cabals <- mapM (withNeed readCabal) cabalFiles
        let dirs = concatMap srcDirs cabals
        cmd (Traced "hlint") "stack exec hlint --" dirs

    phony "build" $ do
        () <- cmd (Traced "clean") "stack clean"
        () <- cmd (Traced "build") ("stack build --test --bench --haddock " ++
            "--ghc-options \"-Werror\" --no-run-benchmarks")
        () <- cmd "rm -rf src/highlight.js src/style.css"
        putNormal "Build Successful"

    phony "check-changelog" $ do
        version <- getVersion
        changelog <- withNeed readMarkdown "changelog.md"
        let (Header 2 header) = index changelog 1
        let [version', date] = map T.unpack $ T.words $ inlinesText header
        assertEqual "Changelog Version" version version'
        time <- liftIO getCurrentTime
        let today = formatTime defaultTimeLocale "(%Y-%m-%d)" time
        assertEqual "Changelog Date" today date
        putNormal "Changelog OK"

getVersion :: Action String
getVersion = do
    [cabalFile] <- getDirectoryFiles "" ["*.cabal"]
    package <- withNeed readCabal cabalFile
    return $ showVersion $ packageVersion package
