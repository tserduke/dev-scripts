module Main (main) where

import Data.Maybe
import Data.Yaml
import Development.Shake
import GHC.Generics


main :: IO ()
main = shakeArgs shakeOptions $
    phony "lint" $ do
        config <- readYaml "stack.yaml" :: Action StackConfig
        need []
        cmd  "echo 1"


data StackConfig = StackConfig
    { packages :: [FilePath]
    } deriving (Generic, FromJSON)


readYaml :: (FromJSON a) => FilePath -> Action a
readYaml file = need [file] >> liftIO (unwrap <$> decodeFile file) where
    unwrap = fromMaybe (error $ "readYaml " ++ file)
