{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 05 (Sat) 11:05:30 by Harold Carr.
-}
module Service.Interact
       (
         G
       , P
       , gp
       , inputS
       , mkInvalidMsgResponse
       , mkValidMethodOrRoute
       )
       where

import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar,
                                          takeMVar)
import           Data.Aeson              (decode)
import qualified Data.Map                as Map
import           Data.String.Conversions (convertString)
import           DataTypes

challenges :: [(String,String)]
challenges = [ ("NOT USED", "NOT USED")
             , ("foldC1"  , "foldA1")
             , ("foldC2"  , "foldA2")
             , ("foldC3"  , "foldA4")
             ]

challenge = ce fst
expect    = ce snd
ce f n = f (challenges!!n)

type G = Name -> IO (Maybe User)
type P = Name -> User -> IO User
type GP = (G, P)

gp :: IO GP
gp = do
    mv <- newEmptyMVar;
    putMVar mv Map.empty
    return (gu mv, pu mv)

gu :: Ord k => MVar (Map.Map k a) -> k -> IO (Maybe a)
gu mv name = do
    m <- takeMVar mv
    let u = Map.lookup name m
    putMVar mv m
    return u

pu :: Ord k => MVar (Map.Map k b) -> k -> b -> IO b
pu mv name user = do
    m <- takeMVar mv
    let newM = Map.insert name user m
    putMVar mv newM
    return user

inputS gu pu s =
    case decode (convertString s) of
        Nothing  -> return Nothing
        (Just d) -> do r <- input gu pu d; return (Just r)

input :: G -> P -> In -> IO Msg
input gu pu i@(In name _) = do
    r <- gu name
    case r of
        Nothing    -> newUser gu pu i
        Just exist -> existingUser gu pu i exist

newUser :: Monad m => t -> (Name -> User -> m a) -> In -> m Msg
newUser gu pu (In name _) = do
    let msgId = 1
    let user = User name msgId
    pu name user
    return (mkMsg msgId)

mkMsg :: Int -> Msg
mkMsg n = Msg n (challenge n)

mkInvalidMsgResponse :: IO Msg
mkInvalidMsgResponse = return $ Msg 0 "INVALID INPUT MESSAGE"

mkValidMethodOrRoute :: IO Msg
mkValidMethodOrRoute = return $ Msg 0 "INVALID HTTP METHOD OR ROUTE"

existingUser :: Monad m => t -> (Name -> User -> m a) -> In -> User -> m Msg
existingUser gu pu   (In name (Msg inId msg))    u@(User _ outId) =
    if inId /= outId ||
       msg /= expect outId
    then return (mkMsg outId)
    else updateUser pu u

updateUser :: Monad m => (Name -> User -> m a) -> User -> m Msg
updateUser pu (User name msgId) = do
    let newId = msgId + 1
    let newUser = User name newId
    pu name newUser
    return (mkMsg newId)

test :: IO ()
test = do
    (gu,pu) <- gp
--    gu (Name "Harold")
--    pu (Name "Harold") (User (Name "Harold"))
    one <- input gu pu (In "Harold" (Msg (-1) "my name is harold"))
    two <- input gu pu (In "Harold" (Msg    0 "my name is harold"))
    thr <- input gu pu (In "Harold" (Msg    0 "foldA1"))
    fou <- input gu pu (In "Harold" (Msg    1 "bad"))
    fiv <- input gu pu (In "Harold" (Msg    1 "foldA2"))
    mapM_ print [one,two,thr,fou,fiv]

{-
(decode (convertString  "{ \"name\": \"H\", \"msg\": {\"txt\":\"foldC3\",\"msgId\":2}  }")) :: (Maybe In)
-}
