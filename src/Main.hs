module Main (main) where

import Development.Shake

main :: IO ()
main = shakeArgs shakeOptions $
    phony "lint" $
        cmd  "echo 1"
