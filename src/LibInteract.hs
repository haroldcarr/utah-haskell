module LibInteract where

import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar,
                                          takeMVar)
import qualified Data.Map                as Map

{-
Created       : 2015 Sep 02 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 03 (Thu) 11:14:51 by Harold Carr.
-}

newtype Name  = Name  String             deriving (Eq, Ord, Show)
newtype MsgId = MsgId Int                deriving (Show)
newtype Msg   = Msg   [String]           deriving (Show)
data In       = In    Name     MsgId Msg deriving (Show)
data Out      = Out   MsgId    Msg       deriving (Show)
data User     = User  Name               deriving (Show)

gp = do
    mv <- newEmptyMVar;
    putMVar mv Map.empty
    return
        (
            -- gu :: Name -> IO (Maybe User)
            (\name -> do
                m <- takeMVar mv
                let u = (Map.lookup (name::Name) m)::(Maybe User)
                putMVar mv m
                return u)
        ,
             --  pu :: Name -> User -> IO User
            (\name user -> do
                m <- takeMVar mv
                let newM = Map.insert name user m
                putMVar mv newM
                return user)
        )

{-
(gu,pu) <- gp
gu (Name "Harold")
pu (Name "Harold") (User (Name "Harold"))

input :: In -> IO Out
input (In name msgId msg) = do
    us <- takeMVar users
    return ()
-}
