{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 05 (Sat) 10:51:13 by Harold Carr.
-}
module LibScotty
( sMain
)
where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Text.Lazy          (unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           LibInteract             as LI
import           Network.HTTP.Types      (badRequest400, ok200)
import           Reactive.Threepenny     (Handler (..))
import           Web.Scotty

sMain :: LI.G -> LI.P -> (String -> IO a) -> IO ()
sMain gu pu handler = scotty 3000 $ do
    post "/" $ do
        b <- body
        let d = decodeUtf8 b
        let msg = unpack d
        liftIO (handler msg)
        r <- liftIO (LI.inputS gu pu d)
        case r of
            Nothing  -> do status badRequest400
                           r <- liftIO LI.mkInvalidMsgResponse
                           json r
            (Just r) -> do status ok200
                           json r
    matchAny "/:everythingElse" $ do
        status badRequest400
        r <- liftIO LI.mkValidMethodOrRoute
        json r
