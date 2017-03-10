module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

addSpinButton :: HBox -> String -> Double -> Double -> IO SpinButton
addSpinButton box name minValue maxValue = do
    vbox  <- vBoxNew False 0
    boxPackStart box vbox PackRepel 0
    label <- labelNew (Just name)
    miscSetAlignment label 0.0 0.5
    boxPackStart vbox label PackNatural 0
    spinb <- spinButtonNewWithRange minValue maxValue 1.0
    boxPackStart vbox spinb PackNatural 0
    return spinb


main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle := "CTApp"
               , windowDefaultWidth := 1280
               , windowDefaultHeight := 768 ]
    table <- tableNew 3 3 True
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


    boxWithSpins <- hBoxNew True 10

    raysSpin <- addSpinButton boxWithSpins "Rays number" 100.0 500.0
    scansSpin <- addSpinButton boxWithSpins "Scans number" 5.0 100.0
    tableAttachDefaults table boxWithSpins 1 2 2 3

    fileChooser <- fileChooserWidgetNew FileChooserActionOpen
    tableAttachDefaults table fileChooser 1 3 0 2

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
