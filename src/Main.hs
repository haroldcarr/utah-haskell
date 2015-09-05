module Main where
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 05 (Sat) 11:05:14 by Harold Carr.
-}
import           Control.Concurrent  (forkIO)
import           Reactive.Threepenny (newEvent)
import qualified Service.Interact    as I
import qualified Service.Scotty      as S
import qualified Service.Threepenny  as TP

main :: IO ()
main = do
    (gu, pu) <- I.gp
    (event, handler) <- newEvent
    forkIO (S.sMain gu pu handler)
    TP.tpMain event
