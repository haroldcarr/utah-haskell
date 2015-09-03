module LibInteract where

import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar,
                                          takeMVar)
import qualified Data.Map                as Map

{-
Created       : 2015 Sep 02 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 03 (Thu) 12:00:25 by Harold Carr.
-}

newtype Name  = Name  String                 deriving (Eq, Ord, Show)
newtype MsgId = MsgId Int                    deriving (Show)
newtype Msg   = Msg   String                 deriving (Show)
data In       = In    Name   MsgId      Msg  deriving (Show)
data Out      = Out   MsgId  Challenges      deriving (Show)
data User     = User  Name                   deriving (Show)

type Challenges = [String]

challenges :: Challenges
challenges = ["welcome"]

type G = Name -> IO (Maybe User)
type P = Name -> User -> IO User
type GP = (G, P)

gp :: IO GP
gp = do
    mv <- newEmptyMVar;
    putMVar mv Map.empty
    return (gu mv, pu mv)

gu :: MVar (Map.Map Name User) -> Name -> IO (Maybe User)
gu mv name = do
    m <- takeMVar mv
    let u = Map.lookup name m
    putMVar mv m
    return u

pu :: MVar (Map.Map Name User) -> Name -> User -> IO User
pu mv name user = do
    m <- takeMVar mv
    let newM = Map.insert name user m
    putMVar mv newM
    return user

input :: G -> P -> In -> IO Out
input gu pu i@(In name _ _) = do
    r <- gu name
    case r of
        Nothing    -> inNew gu pu i
        Just exist -> inExisting gu pu i exist

inNew      :: G -> P -> In -> IO Out
inNew gu pu (In name _ _) = do
    let user = User name
    pu name user
    return (Out (MsgId 0) challenges)

inExisting :: G -> P -> In -> User -> IO Out
inExisting gu pu i u = undefined

{-
(gu,pu) <- gp
gu (Name "Harold")
pu (Name "Harold") (User (Name "Harold"))
input gu pu (In (Name "Harold") (MsgId 3) (Msg "my name is harold"))
-}
