{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LibScotty
( scottyMain
)
where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Monoid             (mconcat)
import           Data.Text.Lazy          (unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           GHC.Generics
import           Reactive.Threepenny     (Handler (..))
import           Web.Scotty


data User = User { userId :: Int, userName :: String } deriving (Eq, Generic, Show)
bob       = User 1 "bob"
jenny     = User 2 "jenny"
allUsers  = [bob, jenny]

instance ToJSON   User
instance FromJSON User

scottyMain :: Handler String -> IO ()
scottyMain handler = scotty 3000 $ do
    get "/users" $ do
        liftIO (handler "/users")
        json allUsers
    get "/users/:id" $ do
        id <- param "id"
        liftIO (handler ("/users/" ++ show id))
        json $ filter ((==id) . userId) allUsers
    post "/reg" $ do
        e <- param "email" `rescue` const next
        b <- body
        liftIO (handler ("/reg " ++ show e ++ " body: " ++ unpack (decodeUtf8 b)))
        html $ mconcat [ "ok ", e ]
    get "/:word" $ do
        beam <- param "word"
        liftIO (handler ("/* " ++ show beam))
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
