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


lint :: Action ()
lint = do
    StackConfig {..} <- withNeed readYaml "stack.yaml"
    let patterns = map (</> "*.cabal") $ fromMaybe ["."] packages
    cabals <- getDirectoryFiles "" patterns
    dirs <- concat <$> mapM (withNeed srcDirs) cabals
    cmd (Traced "hlint") "stack exec hlint --" dirs


checkVersion :: Action ()
checkVersion = do
    changelog <- withNeed readMarkdown "changelog.md"
    let (Header 2 xs) = index changelog 1
    putNormal $ show (inlinesText xs)
    return ()
