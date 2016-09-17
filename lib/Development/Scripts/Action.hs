{-# LANGUAGE RecordWildCards #-}

module Development.Scripts.Action
    ( lint
    , checkVersion
    ) where

import Development.Shake
import Development.Shake.FilePath

import Development.Scripts.Cabal
import Development.Scripts.Markdown
import Development.Scripts.Stack

import Data.Maybe (fromMaybe)
import Data.Sequence (index)
import Development.Scripts.Shake (withNeed)
import qualified Data.Text as T


lint :: Action ()
lint = do
    StackConfig {..} <- withNeed readYaml "stack.yaml"
    let patterns = map (</> "*.cabal") $ fromMaybe ["."] packages
    cabalFiles <- getDirectoryFiles "" patterns
    cabals <- mapM (withNeed readCabal) cabalFiles
    let dirs = concatMap srcDirs cabals
    cmd (Traced "hlint") "stack exec hlint --" dirs


checkVersion :: Action ()
checkVersion = do
    [cabalFile] <- getDirectoryFiles "" ["*.cabal"]
    package <- withNeed readCabal cabalFile
    let version = packageVersion package
    changelog <- withNeed readMarkdown "changelog.md"
    let (Header 2 header) = index changelog 1
    let [version', date] = T.words $ inlinesText header
    liftIO $ print (version, version')
    return ()
