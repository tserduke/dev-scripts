module Main (main) where

import Development.Scripts.Action
import Development.Shake (phony, shakeArgs, shakeOptions)

main :: IO ()
main = shakeArgs shakeOptions $ do
    phony "lint" lint
    phony "build" build
