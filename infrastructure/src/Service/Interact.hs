{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 13 (Sun) 16:50:43 by Harold Carr.
-}
module Service.Interact
       (
         GetUser
       , PutUser
       , getUserPutUser
       , input
       , mkInvalidMethodOrRoute
       , mkInvalidMsgResponse
       )
       where

import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar,
                                          takeMVar)
import           Data.Aeson              (decode)
import qualified Data.Map                as Map
import           Data.String.Conversions (convertString)
import           Msg

data User  = User Name MsgId

challenges :: [(String,String)]
challenges = [ ("NOT USED", "NOT USED")
             , ("r ^. responseHeader \"Content-Type\""  , "Just \"application/json; charset=utf-8\"")
             , ("r ^. responseHeader \"Server\""        , "Just \"Warp/3.0.13.1\"")
             , ("r ^? responseBody . key \"msgId\""     , "Just (Number 3.0)")
             , ("msgId (fromJust (decode (fromJust (r ^? responseBody)) :: Maybe Msg))"
                                                        ,   "4")
             , ("sum [1 .. 5]"                          ,  "15")
             , ("foldr (+) 0 [1 .. 5]"                  ,  "15")
             , ("product [1 .. 5]"                      , "120")
             , ("foldr (*) 1 [1 .. 5]"                  , "120")
             , ("concat [[1,2],[3,4]]"                  , "[1,2,3,4]")
             , ("foldr (++) [] [[1,2],[3,4]]"           , "[1,2,3,4]")
             , ("length [3,2,1]"                        ,   "3")
             , ("foldl (const . succ) 0 [3,2,1]"        ,   "3")
             , ("lastDigit 2038"                        ,   "8")
             , ("dropLastDigit 2038"                    , "203")
             , ("sumDigits [11, 6, 19, 5]"              ,  "23")
             , ("YOU WIN!"                              , "dGVzdDp1c2Vy")
             ]

challenge = ce fst
expect    = ce snd
ce f n = f (challenges!!n)

type GetUser = Name ->         IO (Maybe User)
type PutUser = Name -> User -> IO        User

getUserPutUser :: IO (GetUser, PutUser)
getUserPutUser = do
    mv <- newEmptyMVar;
    putMVar mv Map.empty
    return (getUser mv, putUser mv)

getUser :: Ord k => MVar (Map.Map k a) -> k -> IO (Maybe a)
getUser mv name = do
    m <- takeMVar mv
    let u = Map.lookup name m
    putMVar mv m
    return u

putUser :: Ord k => MVar (Map.Map k b) -> k -> b -> IO b
putUser mv name user = do
    m <- takeMVar mv
    let newM = Map.insert name user m
    putMVar mv newM
    return user

input :: GetUser -> PutUser -> Msg -> IO Msg
input getUser putUser m@(Msg name _ _) = do
    r <- getUser name
    case r of
        Nothing    -> newUser      getUser putUser m
        Just exist -> existingUser getUser putUser m exist

newUser :: Monad m => t -> (Name -> User -> m a) -> Msg -> m Msg
newUser getUser putUser (Msg name _ _) = do
    let msgId = 1
    let user  = User name msgId
    putUser name user
    return (mkMsg name msgId)

existingUser :: Monad m => t -> (Name -> User -> m a) -> Msg -> User -> m Msg
existingUser getUser putUser   (Msg name inId msg)    u@(User _ outId) =
    if inId /= outId ||
       msg  /= expect outId
    then return (mkMsg name outId)
    else updateUser putUser u

updateUser :: Monad m => (Name -> User -> m a) -> User -> m Msg
updateUser putUser (User name msgId) = do
    let newId   = msgId + 1
    let newUser = User name newId
    putUser name newUser
    return (mkMsg name newId)

mkMsg :: Name -> Int -> Msg
mkMsg name n = Msg name n (challenge n)

mkInvalidMsgResponse   :: IO Msg
mkInvalidMsgResponse   = mkInvalidResponse "INVALID INPUT MESSAGE"

mkInvalidMethodOrRoute :: IO Msg
mkInvalidMethodOrRoute = mkInvalidResponse "INVALID HTTP METHOD OR ROUTE"

mkInvalidResponse      :: String -> IO Msg
mkInvalidResponse      = return . Msg "BAD" 0

test :: IO ()
test = do
    (getUser,putUser) <- getUserPutUser
--    getUser (Name "Harold")
--    putUser (Name "Harold") (User (Name "Harold"))
    one <- input getUser putUser (Msg "H" (-1) "my name is h")
    two <- input getUser putUser (Msg "H"    0 "my name is h")
    thr <- input getUser putUser (Msg "H"    1 "Just \"application/json; charset=utf-8\"")
    fou <- input getUser putUser (Msg "H"    1 "Just \"Warp/3.0.13.1\"")
    fiv <- input getUser putUser (Msg "H"    2 "Just \"Warp/3.0.13.1\"")
    mapM_ print [one,two,thr,fou,fiv]

{-
(decode (convertString  "{ \"name\": \"H\", \"msg\": {\"txt\":\"foldC3\",\"msgId\":2}  }")) :: (Maybe Msg)
-}
