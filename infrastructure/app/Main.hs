module Main where
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 05 (Sat) 11:05:14 by Harold Carr.
-}
import           Control.Concurrent   (forkIO)
import           Reactive.Threepenny  (newEvent)
import qualified Service.Display      as D
import qualified Service.Interact     as I
import qualified Service.UserEndpoint as U

main :: IO ()
main = do
    (gu, pu) <- I.gp
    (event, handler) <- newEvent
    forkIO (U.ueMain gu pu handler)
    D.dMain event
