-- TODO: push upstream
module Development.Scripts.Shake
    ( execute
    , withNeed
    , assertEqual
    ) where

import Control.Exception (SomeException, handle)
import Control.Monad (unless)
import Development.Shake (Action, action, liftIO, need, shake, shakeOptions, putQuiet)


execute :: Action a -> IO ()
execute = handle onError . shake shakeOptions . action where
    onError = const $ putStrLn "Fail" :: SomeException -> IO ()


withNeed :: (FilePath -> IO a) -> FilePath -> Action a
withNeed func file = need [file] >> liftIO (func file)

assertEqual :: (Eq a, Show a) => String -> a -> a -> Action ()
assertEqual title expected actual = unless (actual == expected) $
    putQuiet (title ++ "\n" ++ "expected: " ++ show expected ++ "\n but got: " ++ show actual)
