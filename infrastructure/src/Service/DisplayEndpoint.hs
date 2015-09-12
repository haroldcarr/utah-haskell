{-# LANGUAGE OverloadedStrings #-}
{-
Created       : 2015 Sep 02 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2015 Sep 12 (Sat) 11:41:42 by Harold Carr.
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
                elInput <- UI.input # set value str
                                    # set style [("width","800px")]
                liftIO $ modifyIORef inputs (elInput:)

            addInputRedoLayout str = do
                addInput str
                redoLayout

        onEvent event addInputRedoLayout
        addInputRedoLayout "Hello Utah Haskell"

