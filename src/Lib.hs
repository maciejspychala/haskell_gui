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

rowFilter :: Monad m => Array U DIM1 Double -> m (Array D DIM1 Double)
rowFilter row = do
    rowComplex <- computeP $ R.map (\x -> x :+ 0 ) row
    let fftF = fft $ rowComplex
        (Z :. w) = extent fftF
        myfilter tresh v
            | v < tresh = (fromIntegral v) * 0.7
            | otherwise = 0
        barier = round (fromIntegral w/2)
        filtered = fromListUnboxed (Z :. w) $ (P.map (myfilter barier) [1..w])
        fftFilt = R.zipWith (*) fftF filtered
    ifftF <- fmap ifft $ computeP fftFilt
    return $ R.map (\a -> if a<0 then 0 else (if a > 1 then 1 else a)) $ R.map realPart ifftF

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

        renderer x y = let
            p = listOfP (x, y)
            p_zip = zip p [0..(h-1)]
            p_clean = [(round a, b) | (a, b) <- p_zip, a >= 0, a <= fromIntegral (w-1)]
            pixelList = P.map (\(p, h) -> img ! (Z :. p :. h)) p_clean
            pixelSum = sum pixelList
            in dToPx $ pixelSum / (fromIntegral $ length pixelList)

    writePng "res/reconstruct.png" $ generateImage (renderer) originalW originalW


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
    let step = pi / fromIntegral nsteps
        angle = takeWhile (<pi) [x * step | x <- [0..]]
    putStrLn "Calculating projections"
    let listOfP = P.map (\v -> ((fromIntegral v) * p) - tresh) [0..nrays-1]
        rest = P.map (\a -> P.map (\p -> getPixelLine a p w img) listOfP) angle
        maxx = maximum (P.map maximum rest)
        trans = fromListUnboxed (Z :. length rest :. length (head rest)) $ concat $ P.map (P.map (/maxx)) rest
        trans' = transpose trans
    nofilter <- computeUnboxedP trans'
    arraySaveToImage nofilter "nofilter"
    result <- mapRows rowFilter nofilter
    arraySaveToImage result "filter"
    reconstruct result p w

dToPx x = PixelYA8 (round (x * 255)) 255
dToPxNormal x = PixelYA8 (round x) 255

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
