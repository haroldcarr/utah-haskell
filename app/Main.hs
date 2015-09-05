module Main where
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 05 (Sat) 10:49:23 by Harold Carr.
-}
import           Control.Concurrent  (forkIO)
import           LibInteract         as I
import           LibScotty           as S
import           LibThreepenny       as TP
import           Reactive.Threepenny (newEvent)

main :: IO ()
main = do
    (gu, pu) <- I.gp
    (event, handler) <- newEvent
    forkIO (S.sMain gu pu handler)
    TP.tpMain event
