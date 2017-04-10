module Lib where

import Codec.Picture
import System.Directory
import System.IO
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.FFTW
import Data.Complex (Complex(..), realPart, magnitude)
import Prelude as P
import Control.Monad

normalize :: Monad m => Array U DIM2 Double -> m (Array D DIM2 Double)
normalize arr = do
    minn <- foldAllP min 1 arr
    let arrmin = R.map (+ (-minn)) arr
    maxx <- foldAllP max 0 arrmin
    let arrnorm = R.map (/maxx) arrmin
    return arrnorm

myfilter tresh v
    | v < tresh = fromIntegral v * 0.6
    | otherwise = 0

rowFilter :: Monad m => Array D DIM1 Double -> m (Array D DIM1 Double)
rowFilter row = do
    rowComplex <- computeP $ R.map (\x -> x :+ 0 ) row
    let fftF = fft $ rowComplex
        (Z :. w) = extent fftF
        barier = round (fromIntegral w/2)
        filtered = fromListUnboxed (Z :. w) $ (P.map (myfilter barier) [1..w])
        fftFilt = R.zipWith (*) fftF filtered
    ifftF <- fmap ifft $ computeP fftFilt
    return $ R.map realPart ifftF

getRow :: Array D DIM2 Double -> Int -> Array D DIM1 Double
getRow array n = slice array (Any :. n :. All)

mapRows :: Monad m => (Array D DIM1 Double -> m (Array D DIM1 Double)) -> Array D DIM2 Double -> m (Array U DIM2 Double)
mapRows func array = do
    let (Z :. w :. h) = extent array
    rows <- mapM (\num -> do
        let row = getRow array num
        func row) [0,1..(w-1)]
    let hugeRow = toList $ foldr1 append rows
    return $ fromListUnboxed (Z :. w :. h) hugeRow

reconstruct :: Array D DIM2 Double -> Double -> Int -> IO ()
reconstruct img p orgW = do
    let (Z :. w :. h) = extent img
        angleStep = pi / fromIntegral h
        anglesList = takeWhile (<pi) [a * angleStep | a <- [0..]]
        orgWNum = fromIntegral orgW

        listOfIndicies (x, y) = let
            list = P.map (\a -> round $ (x * sin a + y * cos a)/p + orgWNum/2) anglesList
            list_zip = zip list [0..(h-1)]
            in [(a,b) | (a,b) <- list_zip, a >= 0, a < w]

        render (x, y) = let
            list = listOfIndicies (x, y)
            pixelList = P.map (\(p, h) -> img ! (Z :. p :. h)) list
            pixelSum = sum pixelList
            avg = pixelSum / (fromIntegral $ length pixelList)
            in if avg > 0 then avg else 0

    let imageIndicies = [orgWNum/(-2)..orgWNum/2-1]
        img' = fromListUnboxed (Z :. orgW :. orgW) (P.map render [(a,b) | a <- imageIndicies , b <- imageIndicies])
    img <- normalize img'
    writePng "res/reconstruct.png" $ generateImage (\x y -> dToPx (img ! (Z :. x :. y))) orgW orgW


getY a p r x = (x, round ((p - (fromIntegral (x - r) * (cos a))) / (sin a)) + r)
getX a p r y = (round ((p - (fromIntegral (y - r) * (sin a))) / (cos a)) + r, y)

filterCoords r (x, y) = ((fromIntegral x - r)**2 + (fromIntegral y - r)**2) < r**2

getLineAvg :: Double -> Double -> Int -> (Double -> Double -> Int -> Int -> (Int, Int)) -> Array U DIM2 Double -> Double
getLineAvg a p w getCoord img =
    let pixelList = P.map (getCoord a p (div w 2)) [0..w-1]
        r = (fromIntegral w) / 2
        pixelList' = filter (filterCoords r) pixelList
        len = length pixelList'
        ret = (sum $ P.map (\(x, y) -> img ! (Z :. x :. y)) pixelList') / fromIntegral len
    in if isNaN ret then 0 else ret

getDetectorValue :: Double -> Double -> Int -> Array U DIM2 Double -> Double
getDetectorValue a p w img
    | a < pi/4 || a > (3 * pi)/4 = getLineAvg a p w getX img
    | otherwise = getLineAvg a p w getY img

processImage :: String -> Int -> Int -> Int -> IO ()
processImage fname nsteps nrays opening' = do 
    (w, h, img) <- imageToArray fname 
    putStrLn "Image loaded"
    let opening = (fromIntegral opening'/ 180) * pi
        r = fromIntegral w/2
        tresh = sin (opening/2) * r
        p = (sin (opening/2) * r) * 2 / (fromIntegral nrays)
        step = pi / fromIntegral nsteps
        angle = takeWhile (<pi) [x * step | x <- [0..]]
    putStrLn "Calculating projections"
    let listOfP = P.map (\v -> ((fromIntegral v) * p) - tresh) [0..nrays-1]
        rest = P.map (\a -> P.map (\p -> getDetectorValue a p w img) listOfP) angle
        trans = fromListUnboxed (Z :. length rest :. length (head rest)) $ concat $ rest
        trans' = transpose trans
    x <- computeUnboxedP trans'
    nofilter <- normalize x
    arraySaveToImage nofilter "nofilter"
    result' <- mapRows rowFilter nofilter
    result <- normalize result'
    arraySaveToImage result "filter"
    reconstruct result p w

dToPx x
    | x < 0 = PixelYA8 0 255
    | x > 1 = PixelYA8 255 255
    | otherwise = PixelYA8 (round (x * 255)) 255

arraySaveToImage :: Array D DIM2 Double -> String -> IO ()
arraySaveToImage arr fname = do
    let (Z :. w :. h) = extent arr
    writePng ("res/" P.++ fname P.++ ".png") $ generateImage (\x y -> dToPx $ arr ! (Z :. x :. y)) w h

rgbToGrey :: PixelRGB8 -> Double
rgbToGrey (PixelRGB8 r g b) =
  let rgb = P.zipWith (*) [0.2126, 0.7152, 0.0722] $ P.map (fromIntegral) [r, g, b] 
  in fromInteger . round . sum $ rgb :: Double

imageToArray :: String -> IO (Int, Int, Array U DIM2 Double)
imageToArray fname = do
    Right imgRGB <- readImage fname
    let (imgGray@(Image w h _)) = convertRGB8 imgRGB
        arr = fromFunction (Z :. w :. h) (\(Z :. x :. y) -> rgbToGrey $ pixelAt imgGray x y)
    img <- computeUnboxedP arr
    return (w, h, img)
