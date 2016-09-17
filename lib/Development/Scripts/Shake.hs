-- TODO: push upstream
module Development.Scripts.Shake
    ( withNeed
    , assertEqual
    ) where

import Development.Shake

import Control.Monad (unless)


withNeed :: (FilePath -> IO a) -> FilePath -> Action a
withNeed func file = need [file] >> liftIO (func file)


assertEqual :: (Eq a, Show a) => String -> a -> a -> Action ()
assertEqual message expected actual = unless (expected == actual) $
    error (message ++ "\nexpected: " ++ show expected ++ "\n but got: " ++ show actual)
