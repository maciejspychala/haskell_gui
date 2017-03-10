module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle := "CTApp"
               , windowDefaultWidth := 640
               , windowDefaultHeight := 480 ]
    table <- tableNew 2 3 True
    containerAdd window table
    buttonOriginal <- buttonNewWithLabel "original"
    buttonTransformed <- buttonNewWithLabel "transformed"
    buttonDiff <- buttonNewWithLabel "diff"
    buttonQuit <- buttonNewWithLabel "quit"
    onClicked buttonQuit mainQuit

    numberOfAngles <- adjustmentNew 10.0 5.0 100.0 1.0 1.0 0.0
    numberOfRays <- adjustmentNew 10.0 5.0 100.0 1.0 1.0 0.0

    tableAttachDefaults table buttonOriginal 0 1 0 1
    tableAttachDefaults table buttonTransformed 0 1 1 2
    tableAttachDefaults table buttonDiff 0 1 2 3
    tableAttachDefaults table buttonQuit 1 2 0 1



    boxWithSliders <- vBoxNew True 10
    raysScale  <- hScaleNew numberOfRays
    angleScale  <- hScaleNew numberOfAngles
    boxPackStart boxWithSliders raysScale PackGrow 10
    boxPackStart boxWithSliders angleScale PackGrow 10

    tableAttachDefaults table boxWithSliders 1 2 1 2

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
