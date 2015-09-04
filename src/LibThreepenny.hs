{-# LANGUAGE OverloadedStrings #-}

module LibThreepenny
( threepennyMain
)
where

import           Control.Applicative
import           Control.Concurrent          (forkIO)
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Network
import           System.IO                   (hClose)

threepennyMain :: Event String -> IO ()
threepennyMain event =
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
                liftIO $ modifyIORef inputs (elInput:)

            addInputRedoLayout str = do
                addInput str
                redoLayout

        onEvent event addInputRedoLayout
        addInputRedoLayout ""
