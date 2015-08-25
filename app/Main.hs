module Main where

import           Control.Concurrent (forkIO)
import           LibScotty
import           LibThreepenny

main :: IO ()
main = do
    forkIO scottyMain
    threepennyMain
