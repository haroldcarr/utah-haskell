module Main where
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 12 (Sat) 12:18:57 by Harold Carr.
-}
import           Control.Concurrent      (forkIO)
import           Reactive.Threepenny     (newEvent)
import qualified Service.DisplayEndpoint as D
import qualified Service.Interact        as I
import qualified Service.UserEndpoint    as U

main :: IO ()
main = do
    (getUser, putUser) <- I.getUserPutUser
    (event, handler)   <- newEvent
    forkIO (U.ueMain getUser putUser handler)
    D.dMain event
