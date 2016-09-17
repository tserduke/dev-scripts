-- TODO: push upstream
module Development.Scripts.Shake
    ( execute
    , withNeed
    ) where

import Development.Shake

import Control.Exception (SomeException (..), handle)
import Data.Typeable (cast)
import Test.HUnit.Lang (HUnitFailure (..))


execute :: Action a -> IO ()
execute = handle handler . shake shakeOptions . action where
    handler (ShakeException _ _ (SomeException e)) =
        putStrLn $ maybe "Fail" showFailure (cast e)

showFailure :: HUnitFailure -> String
showFailure (HUnitFailure loc msg) = msg ++ "\n" ++ maybe "" show loc


withNeed :: (FilePath -> IO a) -> FilePath -> Action a
withNeed func file = need [file] >> liftIO (func file)
