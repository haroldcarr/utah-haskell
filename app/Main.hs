module Main where

import           Control.Concurrent  (forkIO)
import           LibInteract
import           LibScotty
import           LibThreepenny
import           Reactive.Threepenny (newEvent)

main :: IO ()
main = do
    (gu, pu) <- gp
    (event, handler) <- newEvent
    forkIO (scottyMain gu pu handler)
    threepennyMain event
