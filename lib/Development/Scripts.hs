{-# LANGUAGE RecordWildCards #-}

module Development.Scripts
    ( lint
    , build
    , checkChangelog
    , hackageDocs
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


lint, build, checkChangelog, hackageDocs, publish :: IO ()
lint           = execute "lint"
build          = execute "build"
checkChangelog = execute "check-changelog"
hackageDocs    = execute "hackage-docs"
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
        () <- cmd (Traced "sdist") "stack sdist ."
        () <- cmd (Traced "upload") "stack upload ."
        need ["hackage-docs"]
        () <- cmd (Traced "clean") "hg clean src"

        Stdout bookmark <- cmd "hg bookmark"
        version <- packageVersion <$> getPackage
        () <- cmd (Traced "tag") "hg tag" ("v" ++ version)
        () <- cmd "hg bookmark" (words bookmark !! 1)
        () <- cmd (Traced "push") "hg push"

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
        let stackPath name = fromStdout <$> cmd ("stack path --" ++ name)
        path <- AddEnv "PATH" <$> stackPath "bin-path"
        let buildDir = "--builddir=" ++ temp

        snapshot <- stackPath "snapshot-pkg-db"
        local    <- stackPath "local-pkg-db"
        let pdb = ("--package-db=" ++)
        () <- cmd (RemEnv "GHC_PACKAGE_PATH") path "cabal configure" buildDir
            (pdb "clear") (pdb "global") (pdb snapshot) (pdb local)

        () <- cmd path "cabal haddock --for-hackage" buildDir
            "--haddock-option=--hyperlinked-source"
        pkgId <- packageId <$> getPackage
        () <- cmd path "cabal upload --documentation" (temp </> pkgId ++ "-docs.tar.gz")

        putNormal "Docs uploaded to Hackage"


getPackage :: Action GenericPackageDescription
getPackage = do
    [file] <- getDirectoryFiles "" ["*.cabal"]
    withNeed readCabal file
