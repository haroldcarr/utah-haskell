{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 05 (Sat) 11:06:15 by Harold Carr.
-}
module Service.Scotty
( sMain
)
where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Text.Lazy          (unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Types      (badRequest400, ok200)
import           Reactive.Threepenny     (Handler (..))
import qualified Service.Interact        as I
import           Web.Scotty

sMain :: I.G -> I.P -> (String -> IO a) -> IO ()
sMain gu pu handler = scotty 3000 $ do
    post "/" $ do
        b <- body
        let d = decodeUtf8 b
        let msg = unpack d
        liftIO (handler msg)
        r <- liftIO (I.inputS gu pu d)
        case r of
            Nothing  -> do status badRequest400
                           r <- liftIO I.mkInvalidMsgResponse
                           json r
            (Just r) -> do status ok200
                           json r
    matchAny "/:everythingElse" $ do
        status badRequest400
        r <- liftIO I.mkValidMethodOrRoute
        json r
