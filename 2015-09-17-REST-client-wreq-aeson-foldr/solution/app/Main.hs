module Main where
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 16 (Wed) 21:46:53 by Harold Carr.
-}
import           Control.Concurrent      (forkIO)
import           Reactive.Threepenny     (newEvent)
import qualified Service.DisplayEndpoint as D
import qualified Service.Interact        as I
import qualified Service.UserEndpoint    as U

main :: IO ()
main = do
    (getUser   , putUser)      <- I.getUserPutUser
    (msgEvent  , msgHandler)   <- newEvent
    (adminEvent, adminHandler) <- newEvent
    forkIO (U.ueMain getUser putUser msgHandler adminHandler)
    D.dMain msgEvent adminEvent
