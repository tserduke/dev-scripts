module Development.Scripts.Shake
    ( execute
    , withNeed
    ) where

import Control.Exception (SomeException, handle)
import Development.Shake (Action, action, liftIO, need, shake, shakeOptions)


execute :: Action a -> IO ()
execute = handle onError . shake shakeOptions . action where
    onError = const $ putStrLn "Fail" :: SomeException -> IO ()


-- TODO: push upstream
withNeed :: (FilePath -> IO a) -> FilePath -> Action a
withNeed func file = need [file] >> liftIO (func file)
