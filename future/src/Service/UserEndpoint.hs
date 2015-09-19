{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 17 (Thu) 15:44:26 by Harold Carr.
-}
module Service.UserEndpoint
( ueMain
)
where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (decode, encode)
import           Data.String.Conversions (convertString)
import           Data.Text.Lazy          (unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Types      (badRequest400, ok200)
import qualified Service.Interact        as I
import           Web.Scotty

ueMain :: I.GetUser -> I.PutUser -> (String -> IO a) -> (Int -> IO a) -> IO ()
ueMain getUser putUser displayHandler adminHandler = scotty 3000 $ do
    post "/" $ do
        b <- body
        let d   = decodeUtf8 b
        case decode (convertString d) of
            Nothing  -> do liftIO (displayHandler ("-> " ++ unpack d))
                           status badRequest400
                           r <- liftIO I.mkInvalidMsgResponse
                           liftIO (displayHandler ("<- " ++ convertString (encode r)))
                           json r
            (Just m) -> do liftIO (displayHandler (I.showInput m))
                           r <- liftIO (I.input getUser putUser m)
                           status ok200
                           liftIO (displayHandler (I.showOutput r))
                           json r

    matchAny "/dGVzdDp1c2Vy/:id" $ do
        id <- param "id"
        liftIO (adminHandler ((read id) :: Int))
        status ok200

    matchAny "/invalid-ok200" $ do
        status ok200
        r <- liftIO I.mkInvalidMethodOrRoute
        json r

    matchAny "/:everythingElse" $ do
        status badRequest400
        r <- liftIO I.mkInvalidMethodOrRoute
        json r
