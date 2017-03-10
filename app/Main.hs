module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Lib

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

getFilename :: FileChooserClass fc => fc -> IO String
getFilename fileChooser = do
    file <- fileChooserGetPreviewFilename fileChooser
    case file of
        Nothing -> return "xd.png"
        Just fpath -> return fpath

lol = "siema"


runAlgorithm :: String -> Int -> Int -> Int-> Image -> Image -> IO()
runAlgorithm filepath nsteps nrays alpha target projection = do
    processImage filepath nsteps nrays alpha
    imageSetFromFile target "res/filter.png"
    imageSetFromFile projection "res/reconstruct.png"
    widgetSetSizeRequest target 300 300
    return ()

main :: IO ()
main = do
    void initGUI
    window <- windowNew
    set window [ windowTitle := "CTApp"
               , windowDefaultWidth := 1280
               , windowDefaultHeight := 768 ]
    table <- tableNew 3 3 True
    containerAdd window table
    imgOriginal <- imageNew
    imageSetPixelSize imgOriginal 300
    imgTransformed <- imageNew
    imgProj <- imageNew
    imageSetPixelSize imgProj 420
    buttonQuit <- buttonNewWithLabel "quit"
    onClicked buttonQuit mainQuit

    numberOfAngles <- adjustmentNew 10.0 5.0 100.0 1.0 1.0 0.0
    numberOfRays <- adjustmentNew 10.0 5.0 100.0 1.0 1.0 0.0

    tableAttachDefaults table imgOriginal 0 1 0 1
    tableAttachDefaults table imgTransformed 0 1 1 2
    tableAttachDefaults table imgProj 0 1 2 3


    boxWithSpins <- hBoxNew True 10

    raysSpin <- addSpinButton boxWithSpins "Rays number" 100.0 500.0
    scansSpin <- addSpinButton boxWithSpins "Scans number" 10.0 300.0
    alphaSpin <- addSpinButton boxWithSpins "alpha" 90.0 180.0
    tableAttachDefaults table boxWithSpins 1 2 2 3

    fileChooser <- fileChooserWidgetNew FileChooserActionOpen
    tableAttachDefaults table fileChooser 1 3 0 1

    fileChooserSetPreviewWidget fileChooser imgOriginal

    onUpdatePreview fileChooser $ do
        file <- fileChooserGetPreviewFilename fileChooser
        case file of
            Nothing -> putStrLn "No file selected"
            Just fpath -> do 
                imageSetFromFile imgOriginal fpath
        widgetSetSizeRequest imgOriginal 300 300

    buttonRun <- buttonNewWithLabel "run"
    onClicked buttonRun $ do
        fileName <- getFilename fileChooser
        steps <- spinButtonGetValue scansSpin
        nrays <- spinButtonGetValue raysSpin
        alpha <- spinButtonGetValue alphaSpin
        let stepsInt = round steps :: Int
        let nraysInt = round nrays :: Int
        let alphaInt = round alpha :: Int
        runAlgorithm (fileName) stepsInt nraysInt alphaInt imgTransformed imgProj
    tableAttachDefaults table buttonRun 1 2 1 2

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
