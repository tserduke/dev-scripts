{-# LANGUAGE RecordWildCards #-}

module Development.Scripts.Action
    ( lint
    ) where

import Development.Shake
import Development.Shake.FilePath

import Development.Scripts.Cabal
import Development.Scripts.Stack

import Data.Maybe (fromMaybe)
import Development.Scripts.Shake (withNeed)



lint :: Action ()
lint = do
    StackConfig {..} <- withNeed readYaml "stack.yaml"
    let patterns = map (</> "*.cabal") $ fromMaybe ["."] packages
    cabals <- getDirectoryFiles "" patterns
    dirs <- concat <$> mapM (withNeed srcDirs) cabals
    cmd (Traced "hlint") "stack exec hlint --" dirs
