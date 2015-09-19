{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Sep 02 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 16 (Wed) 22:02:19 by Harold Carr.
-}
module Service.DisplayEndpoint
( dMain
)
where

import           Control.Monad               (void)
import           Data.IORef
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

dMain :: Event String -> Event Int -> IO ()
dMain msgEvent adminEvent =
    startGUI defaultConfig $ \w -> do
        inputs <- liftIO $ newIORef []
        size   <- liftIO $ newIORef 10
        let
            redoLayout :: UI ()
            redoLayout = void $ do
                n      <- liftIO (readIORef size)
                layout <- mkLayout . take n =<< liftIO (readIORef inputs)
                getBody w # set children [layout]

            mkLayout :: [Element] -> UI Element
            mkLayout xs = column $ map element xs

            addInput :: String -> UI ()
            addInput str = do
                eInput  <- UI.string str
                liftIO $ modifyIORef inputs (eInput :)

            addInputRedoLayout str = do
                addInput str
                redoLayout

            changeSizeRedoLayout n = do
                liftIO $ writeIORef size n
                redoLayout

        onEvent msgEvent addInputRedoLayout
        onEvent adminEvent changeSizeRedoLayout
        addInputRedoLayout "Hello Utah Haskell"

