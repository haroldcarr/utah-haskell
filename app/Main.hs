module Main where

import           Control.Concurrent  (forkIO)
import           LibScotty
import           LibThreepenny
import           Reactive.Threepenny (newEvent)

main :: IO ()
main = do
    (event, handler) <- newEvent
    forkIO (scottyMain handler)
    threepennyMain event
