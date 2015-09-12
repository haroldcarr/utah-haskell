{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 12 (Sat) 12:23:28 by Harold Carr.
-}
module Service.UserEndpoint
( ueMain
)
where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (encode)
import           Data.String.Conversions (convertString)
import           Data.Text.Lazy          (unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Types      (badRequest400, ok200)
import           Reactive.Threepenny     (Handler (..))
import qualified Service.Interact        as I
import           Web.Scotty

ueMain :: I.GetUser -> I.PutUser -> (String -> IO a) -> IO ()
ueMain getUser putUser displayHandler = scotty 3000 $ do
    post "/" $ do
        b <- body
        let d   = decodeUtf8 b
        let msg = unpack d
        liftIO (displayHandler ("-> " ++ msg))
        r <- liftIO (I.inputS getUser putUser d)
        case r of
            Nothing  -> do status badRequest400
                           r <- liftIO I.mkInvalidMsgResponse
                           liftIO (displayHandler ("<- " ++ (convertString (encode r))))
                           json r
            (Just r) -> do status ok200
                           liftIO (displayHandler ("<- " ++ (convertString (encode r))))
                           json r
    matchAny "/:everythingElse" $ do
        status badRequest400
        r <- liftIO I.mkInvalidMethodOrRoute
        json r
