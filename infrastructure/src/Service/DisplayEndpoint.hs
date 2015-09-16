{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Sep 02 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 16 (Wed) 08:26:42 by Harold Carr.
-}
module Service.DisplayEndpoint
( dMain
)
where

import           Control.Monad               (void)
import           Data.IORef
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

dMain :: Event String -> IO ()
dMain event =
    startGUI defaultConfig $ \w -> do
        inputs <- liftIO $ newIORef []
        let
            redoLayout :: UI ()
            redoLayout = void $ do
                layout <- mkLayout =<< liftIO (readIORef inputs)
                getBody w # set children [layout]

            mkLayout :: [Element] -> UI Element
            mkLayout xs = column $ map element xs

            addInput :: String -> UI ()
            addInput str = do
                eInput <- UI.string str
                history <- liftIO $ readIORef inputs
                liftIO (if length history > 10
                        then writeIORef  inputs (eInput : take 10 history)
                        else modifyIORef inputs (eInput :))

            addInputRedoLayout str = do
                addInput str
                redoLayout

        onEvent event addInputRedoLayout
        addInputRedoLayout "Hello Utah Haskell"

