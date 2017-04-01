module Lib
    ( processImage2
    ) where

import Codec.Picture
import Data.Either
import System.Directory
import Debug.Trace
import System.IO
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.FFTW
import Data.Complex (Complex(..), realPart)
import Prelude as P
import Control.Monad

geommean array =
    let len = length array
        mul = product array
    in mul ** (1/ fromIntegral len)

rowFilter :: Monad m => Array U DIM1 Double -> m (Array U DIM1 Double)
rowFilter row =
    let fftD = fft $ computeS $ R.map (\x -> x :+ 0 ) row
    in computeUnboxedP $ R.map realPart fftD


xd :: Array U DIM2 Double -> IO ()
xd img = do
    let (Z :. w :. h) = extent img
        angleStep = pi / fromIntegral h
        anglesList = takeWhile (<pi) [a * angleStep | a <- [0..]]
        wNum = fromIntegral w
    let xd = slice img (Any :. w-2 :. All)
    print anglesList
    print w
    print h
    let listOfP (xi, yi) = let
            xd = fromIntegral xi
            yd = fromIntegral yi
            x = xd - wNum/2.0
            y = yd - wNum/2.0
            in P.map (\a -> round((x * (cos a)) + (y * (sin a)) + wNum/2.0)) anglesList    

    let renderer xi yi = let
            p = listOfP (xi, yi)
            p_zip = zip p [0..(h-1)]
            p_clean = [(a, b) | (a, b) <- p_zip, a >= 0, a <= (w-1)]
            pixelList = P.map (\(p, h) -> img ! (Z :. p :. h)) p_clean
            pixelSum = sum pixelList
            in dToPx $ pixelSum / fromIntegral h
    
    --writePng "res/xd.png" $ generateImage (renderer img w h anglesList) w w
    writePng "res/xd.png" $ generateImage (renderer) w w

processImage2 :: String -> Int -> IO ()
processImage2 fname nsteps = do 
  a <- readImage $ fname
  case a of
    Left err -> putStrLn err
    Right img -> do
      putStrLn $ "Image Loaded"
      let grey = pixelMap rgbToGreyscale $ convertRGB8 img
          step = 180.0 / fromIntegral nsteps
          angle = takeWhile (<180) [x * step | x <- [0..]]
      putStrLn "Calculating projections"

      decomposition <- return $ angle >>= repaDecompose grey
      projection <- return $ decomposition >>= repaProject
      result' <- computeUnboxedP $ foldr1 append projection :: IO (Array U DIM2 Double)

      putStrLn "Normalizing result"
      max <- foldAllP max 0 result'
      putStrLn $ "Max: " P.++ show max
      result <- computeUnboxedP $ R.map (/max) result' :: IO (Array U DIM2 Double)
      xd result

      putStrLn "Converting to image"
      let renderer x y = dToPx $ result ! (Z :. x :. y)
          (Z :. w :. h) = extent result

      putStrLn "Saving image"
      writePng "res/result.png" $ generateImage renderer w h
      putStrLn "--DONE"

dToPx x = PixelYA8 (round (x * 255)) 255

repaProject :: (Monad m) => (Array V DIM2 PixelYA8) -> m (Array D DIM2 Double)
repaProject arr = do
    let p (PixelYA8 y a) = fromIntegral y / 255.0 :: Double
    doubles <- computeUnboxedP . transpose $ R.traverse arr id (\f (Z :. i :. j) -> p $ f (Z :. i :. j))
    projected <- foldP (+) 0 doubles
    let (Z :. w) = extent projected
    let dw = fromIntegral w :: Double
    return $ (reshape (Z :. w :. 1) . R.map (/dw)) projected

rotateAround :: (Double, Double) -> (Int, Int) -> Double -> (Int, Int)
rotateAround (cx, cy) pt angle = let
    d = fromIntegral :: Int -> Double
    (rx, ry) =  
      (\(x, y) -> (x * cos angle - y * sin angle, x * sin angle + y * cos angle)) .
      (\(x, y) -> (d x - cx, d y - cy)) $ 
      pt
  in (truncate (rx + cx), truncate (ry + cy))

repaDecompose :: (Monad m) => Image PixelYA8 -> Double -> m (Array V DIM2 PixelYA8)
repaDecompose img angle = let
    w = imageWidth img
    h = imageHeight img

    fi = angle * pi / 180.0

    i = round :: Double -> Int
    d = fromIntegral :: Int -> Double
    
    c = (/2.0) . d
    center@(cx, cy) = (c w, c h)
    
    dHyp = sqrt . d $ w^2 + h^2
    iHyp = round dHyp

    xOffset = i $ (dHyp) / 2.0 - cx
    yOffset = i $ (dHyp) / 2.0 - cy

    toPixel (x, y) = if x >= 0 && y >= 0 && x < w && y < h
      then pixelAt img x y
      else PixelYA8 0 255

    arr = fromListUnboxed (Z :. w :. h) [ (x, y) | x <- [0..(w - 1)], y <-[0..(h - 1)]]
    rotated = R.traverse arr (\(Z :. a :. b) -> (Z :. iHyp :. iHyp)) (\f (Z :. i :. j) -> toPixel $ rotateAround center (i - xOffset, j - yOffset) fi)
    result = computeVectorP rotated
  in result

rgbToGreyscale :: PixelRGB8 -> PixelYA8
rgbToGreyscale (PixelRGB8 r g b) = 
  let rgb = P.zipWith (*) [0.2126, 0.7152, 0.0722] $ P.map (fromIntegral) [r, g, b] 
      value = fromInteger . round . sum $ rgb :: Pixel8
  in (PixelYA8 value 255)
