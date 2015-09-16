{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Aug 26 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 15 (Tue) 20:50:44 by Harold Carr.
-}
module Service.Interact
       (
         GetUser
       , PutUser
       , getUserPutUser
       , input
       , mkInvalidMethodOrRoute
       , mkInvalidMsgResponse
       , showInput
       , showOutput
       )
       where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Map                as Map
import           Msg

data User  = User Name MsgId

challenges :: [(String,String)]
challenges = [ ("NOT USED", "NOT USED")
             , ("r ^. responseHeader \"Content-Type\""  , "application/json; charset=utf-8")
             , ("r ^. responseStatus . statusCode"      , "200")
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

challenge, expect :: Int -> String
challenge = ce fst
expect    = ce snd

-- ce :: forall t. ((String, String) -> t) -> Int -> t
ce f n = f (challenges!!n)

numChallenges :: Int
numChallenges = length challenges - 1

score, showInput, showOutput :: Msg -> String
score m = show (msgId m) ++ "/" ++ show numChallenges
showInput  m = "-> name:" ++ name m ++ ", id:"    ++ show (msgId m)
showOutput m = "<- name:" ++ name m ++ ", score:" ++ show (score m) ++ ", challenge: " ++ txt m

type GetUser = Name ->         IO (Maybe User)
type PutUser = Name -> User -> IO        User

getUserPutUser :: IO (GetUser, PutUser)
getUserPutUser = do
    mv <- newEmptyMVar;
    putMVar mv Map.empty
    return (getUser mv, putUser mv)

getUser :: Ord k => MVar (Map.Map k a) -> k -> IO (Maybe a)
getUser mv name0 = do
    m <- takeMVar mv
    let u = Map.lookup name0 m
    putMVar mv m
    return u

putUser :: Ord k => MVar (Map.Map k b) -> k -> b -> IO b
putUser mv name0 user = do
    m <- takeMVar mv
    let newM = Map.insert name0 user m
    putMVar mv newM
    return user

input :: GetUser -> PutUser -> Msg -> IO Msg
input gUser pUser m@(Msg name0 _ _) = do
    r <- gUser name0
    case r of
        Nothing    -> newUser      pUser m
        Just exist -> existingUser pUser m exist

newUser :: Monad m => (Name -> User -> m a) -> Msg -> m Msg
newUser pUser (Msg name0 _ _) = do
    let mId  = 1
    let user = User name0 mId
    pUser name0 user
    return (mkMsg name0 mId)

existingUser :: Monad m => (Name -> User -> m a) -> Msg -> User -> m Msg
existingUser pUser   (Msg name0 inId msg)    u@(User _ outId) =
    if inId /= outId ||
       msg  /= expect outId
    then return (mkMsg name0 outId)
    else updateUser pUser u

updateUser :: Monad m => (Name -> User -> m a) -> User -> m Msg
updateUser pUser (User name0 mId) = do
    let newId    = mId + 1
    let newUser0 = User name0 newId
    pUser name0 newUser0
    return (mkMsg name0 newId)

mkMsg :: Name -> Int -> Msg
mkMsg name0 n = Msg name0 n (challenge n)

mkInvalidMsgResponse   :: IO Msg
mkInvalidMsgResponse   = mkInvalidResponse "INVALID INPUT MESSAGE"

mkInvalidMethodOrRoute :: IO Msg
mkInvalidMethodOrRoute = mkInvalidResponse "INVALID HTTP METHOD OR ROUTE"

mkInvalidResponse      :: String -> IO Msg
mkInvalidResponse      = return . Msg "BAD" 0

test :: IO ()
test = do
    (gUser,pUser) <- getUserPutUser
--    gUser (Name "Harold")
--    pUser (Name "Harold") (User (Name "Harold"))
    one <- input gUser pUser (Msg "H" (-1) "my name is h")
    two <- input gUser pUser (Msg "H"    0 "my name is h")
    thr <- input gUser pUser (Msg "H"    1 "Just \"application/json; charset=utf-8\"")
    fou <- input gUser pUser (Msg "H"    1 "Just \"Warp/3.0.13.1\"")
    fiv <- input gUser pUser (Msg "H"    2 "Just \"Warp/3.0.13.1\"")
    mapM_ print [one,two,thr,fou,fiv]

{-
(decode (convertString  "{ \"name\": \"H\", \"msg\": {\"txt\":\"foldC3\",\"msgId\":2}  }")) :: (Maybe Msg)
-}
