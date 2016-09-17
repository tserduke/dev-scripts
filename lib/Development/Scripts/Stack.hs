{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Development.Scripts.Stack
    ( StackConfig (StackConfig)
    , packages
    , readYaml
    ) where

import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON, decodeFile)
import GHC.Generics (Generic)


data StackConfig = StackConfig
    { packages :: Maybe [FilePath]
    } deriving (Generic, FromJSON)


readYaml :: (FromJSON a) => FilePath -> IO a
readYaml file = unwrap <$> decodeFile file where
    unwrap = fromMaybe (error $ "readYaml " ++ file)
