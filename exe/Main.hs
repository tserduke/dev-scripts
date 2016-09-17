module Main (main) where

import Development.Scripts (rules)
import Development.Shake (shakeArgs, shakeOptions)

main :: IO ()
main = shakeArgs shakeOptions rules
