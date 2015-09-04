module LibInteract where

import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar,
                                          takeMVar)
import qualified Data.Map                as Map

{-
Created       : 2015 Sep 02 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 04 (Fri) 10:30:46 by Harold Carr.
-}

newtype Name  = Name  String        deriving (Eq, Ord, Show)
newtype MsgId = MsgId Int           deriving (Show)
data    Msg   = Msg   MsgId  String deriving (Show)
data    In    = In    Name   Msg    deriving (Show)
data    User  = User  Name   MsgId  deriving (Show)

challenges :: [(String,String)]
challenges = [ ("foldC1","foldA1")
             , ("foldC2","foldA2")
             , ("foldC3","foldA3")
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

input :: G -> P -> In -> IO Msg
input gu pu i@(In name _) = do
    r <- gu name
    case r of
        Nothing    -> newUser gu pu i
        Just exist -> existingUser gu pu i exist

newUser :: Monad m => t -> (Name -> User -> m a) -> In -> m Msg
newUser gu pu (In name _) = do
    let n = 0
    let msgId = MsgId n
    let user = User name msgId
    pu name user
    return (mkMsg n)

mkMsg :: Int -> Msg
mkMsg n = Msg (MsgId n) (challenge n)

existingUser :: Monad m => t -> (Name -> User -> m a) -> In -> User -> m Msg
existingUser gu pu   (In (Name name) (Msg (MsgId inN) msg))    u@(User _ (MsgId outN)) =
    if inN /= outN ||
       msg /= expect outN
    then return (mkMsg outN)
    else updateUser pu u

updateUser :: Monad m => (Name -> User -> m a) -> User -> m Msg
updateUser pu (User n@(Name name) (MsgId msgId)) = do
    let newId = msgId + 1
    let newMsgId = MsgId newId
    let newUser = User n newMsgId
    pu n newUser
    return (mkMsg newId)

test :: IO ()
test = do
    (gu,pu) <- gp
--    gu (Name "Harold")
--    pu (Name "Harold") (User (Name "Harold"))
    one <- input gu pu (In (Name "Harold") (Msg (MsgId (-1)) "my name is harold"))
    two <- input gu pu (In (Name "Harold") (Msg (MsgId    0) "my name is harold"))
    thr <- input gu pu (In (Name "Harold") (Msg (MsgId    0) "foldA1"))
    fou <- input gu pu (In (Name "Harold") (Msg (MsgId    1) "bad"))
    fiv <- input gu pu (In (Name "Harold") (Msg (MsgId    1) "foldA2"))
    mapM_ print [one,two,thr,fou,fiv]
