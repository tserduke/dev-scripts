module Development.Scripts
    ( lint
    ) where

import qualified Development.Scripts.Action as A
import Development.Scripts.Shake (execute)


lint :: IO ()
lint = execute A.lint
