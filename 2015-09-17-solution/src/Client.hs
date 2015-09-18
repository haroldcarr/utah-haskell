{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Sep 05 (Sat) 11:05:00 by Harold Carr.
Last Modified : 2015 Sep 18 (Fri) 08:07:17 by Harold Carr.
-}
module Client where

import           Control.Lens
import           Data.Aeson   (FromJSON, ToJSON, toJSON)
import           GHC.Generics
import           Network.Wreq

type Name  = String
type MsgId = Int
data Msg   = Msg { name  :: Name
                 , msgId :: MsgId
                 , txt   :: String
                 } deriving (Generic, Show)

instance ToJSON   Msg
instance FromJSON Msg

epAddr :: String
epAddr = "http://127.0.0.1:3000"

mkMsg :: MsgId -> String -> Msg
mkMsg = Msg "H"

msgs :: [Msg]
msgs  = [ mkMsg  0 "hello"
        , mkMsg  1 "application/json; charset=utf-8"
        , mkMsg 19 "200"                    -- wrong id, right answer
        , mkMsg  2 "WRONG ANSWER"           -- right id, wrong answer
        , mkMsg  2 "200"
        , mkMsg  3 "Just (Number 3.0)"
        , mkMsg  4 "4"
        , mkMsg  5 "15"
        , mkMsg  6 "15"
        , mkMsg  7 "120"
        , mkMsg  8 "120"
        , mkMsg  9 "[1,2,3,4]"
        , mkMsg 10 "[1,2,3,4]"
        , mkMsg 11 "3"
        , mkMsg 12 "3"
        , mkMsg 13 "8"
        , mkMsg 14 "203"
        , mkMsg 15 "23"
        , mkMsg 16 "winner"           -- redundant message
        , mkMsg 25 "whatever"         -- ditto
        ]

test :: IO ()
test = do
    rs <- mapM (post epAddr . toJSON) msgs
    mapM_ (\(m, r) -> do putStrLn $ "--> " ++ show m
                         putStrLn $ "<-- " ++ show (r ^? responseBody))
          (zip msgs rs)

-- CIS 194 solutions

-- Exercise 1

lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 4

sumDigits :: [Integer] -> Integer
sumDigits = foldr (\x acc -> dropLastDigit x + lastDigit x + acc) 0
