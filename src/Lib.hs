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

myfilter r tresh v =
    let dist = abs (r - v)
    in if v < tresh
        then fromIntegral v * 0.6
        else 0

normalize :: Monad m => Array U DIM2 Double -> m (Array U DIM2 Double)
normalize arr = do
    minn <- foldAllP min 1 arr
    let arrmin = R.map (+ (-minn)) arr
    maxx <- foldAllP max 0 arrmin
    let arrnorm = R.map (/maxx) arrmin
    computeUnboxedP arrnorm

rowFilter :: Monad m => Array U DIM1 Double -> m (Array D DIM1 Double)
rowFilter row = do
    rowComplex <- computeP $ R.map (\x -> x :+ 0 ) row
    let fftF = fft $ rowComplex
        (Z :. w) = extent fftF
        barier = round (fromIntegral w/2)
        filtered = fromListUnboxed (Z :. w) $ (P.map (myfilter (round (fromIntegral w/2)) barier) [1..w])
        fftFilt = R.zipWith (*) fftF filtered
    ifftF <- fmap ifft $ computeP fftFilt
    return $ R.map realPart ifftF

getRow :: Array U DIM2 Double -> Int -> Array D DIM1 Double
getRow array n = slice array (Any :. n :. All)

mapRows :: Monad m => (Array U DIM1 Double -> m (Array D DIM1 Double)) -> Array U DIM2 Double -> m (Array U DIM2 Double)
mapRows func array = do
    let (Z :. w :. h) = extent array
    rows <- mapM (\num -> do
        let rowD = getRow array num
        row <- computeUnboxedP rowD
        func row) [0,1..(w-1)]
    let hugeRow = toList $ foldr1 append rows
    return $ fromListUnboxed (Z :. w :. h) hugeRow

reconstruct :: Array U DIM2 Double -> Double -> Int -> IO ()
reconstruct img p originalW = do
    let (Z :. w :. h) = extent img
        angleStep = pi / fromIntegral h
        anglesList = takeWhile (<pi) [a * angleStep | a <- [0..]]
        wNum = fromIntegral w
        wOriginalNum = fromIntegral originalW
        ratio = wNum / wOriginalNum

        listOfP (xi, yi) = let
            xd = fromIntegral xi
            yd = fromIntegral yi
            x = xd - wOriginalNum/2.0
            y = yd - wOriginalNum/2.0
            in P.map (\a -> ((x * (cos a)) + (y * (sin a))) + (wNum/2)) anglesList

        renderer (x, y) = let
            p = listOfP (x, y)
            p_zip = zip p [0..(h-1)]
            p_clean = [(round a, b) | (a, b) <- p_zip, a >= 0, a <= fromIntegral (w-1)]
            pixelList = P.map (\(p, h) -> img ! (Z :. p :. h)) p_clean
            pixelSum = sum pixelList
            avg = pixelSum / (fromIntegral $ length pixelList)
            in if avg > 0 then avg else 0

    let img' = fromListUnboxed (Z :. originalW :. originalW) (P.map renderer [(a,b) | a <- [0..originalW-1], b <- [0..originalW-1]])
    img <- normalize img'
    writePng "res/reconstruct.png" $ generateImage (\x y -> dToPx (img ! (Z :. x :. y))) originalW originalW


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

getPixelLine :: Double -> Double -> Int -> Array U DIM2 Double -> Double
getPixelLine a p w img
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
        rest = P.map (\a -> P.map (\p -> getPixelLine a p w img) listOfP) angle
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

arraySaveToImage :: Array U DIM2 Double -> String -> IO ()
arraySaveToImage arr fname = do
    let (Z :. w :. h) = extent arr
    writePng ("res/" P.++ fname P.++ ".png") $ generateImage (\x y -> dToPx $ arr ! (Z :. x :. y)) w h

rgbToGreyDouble :: PixelRGB8 -> Double
rgbToGreyDouble  (PixelRGB8 r g b) = 
  let rgb = P.zipWith (*) [0.2126, 0.7152, 0.0722] $ P.map (fromIntegral) [r, g, b] 
  in fromInteger . round . sum $ rgb :: Double

imageToArray :: String -> IO (Int, Int, Array U DIM2 Double)
imageToArray fname = do
    Right imgD <- readImage fname
    let (img@(Image w h _)) = convertRGB8 imgD 
        arr = fromFunction (Z :. w :. h) (\(Z :. x :. y) -> let p = pixelAt img x y in rgbToGreyDouble p)
    img <- computeUnboxedP arr
    return (w, h, img)
