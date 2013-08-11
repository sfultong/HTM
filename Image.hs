module Image (
) where

import Data.Map (Map)
import qualified Data.Map as M
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Types as Types
import Graphics.UI.SDL.Image as Image
import Foreign
import Control.Monad
import Data.Word
import Data.Bits

getPixel :: Int -> Int -> SDL.Surface -> IO SDL.Pixel
getPixel x y surf = do
  pixels <- castPtr `liftM` Types.surfaceGetPixels surf
  SDL.Pixel `liftM` peekElemOff pixels (x + (y * SDL.surfaceGetWidth surf))

rShift = 0
gShift = 8
bShift = 16
aShift = 24
rMask = 255 `shift` rShift
gMask = 255 `shift` gShift
bMask = 255 `shift` bShift
aMask = 255 `shift` aShift
getR :: SDL.Pixel -> Word8
getR (SDL.Pixel pdata) = fromIntegral $ (pdata .&. rMask) `shift` (negate rShift)
getG :: SDL.Pixel -> Word8
getG (SDL.Pixel pdata) = fromIntegral $ (pdata .&. gMask) `shift` (negate gShift)
getB :: SDL.Pixel -> Word8
getB (SDL.Pixel pdata) = fromIntegral $ (pdata .&. bMask) `shift` (negate bShift)
getA :: SDL.Pixel -> Word8
getA (SDL.Pixel pdata) = fromIntegral $ (pdata .&. aMask) `shift` (negate aShift)

getRGBData :: SDL.Pixel -> (Word8,Word8,Word8)
getRGBData p = (getR p, getG p, getB p)

getPixelActive :: SDL.Pixel -> Bool
getPixelActive p = getR p < 170 && getG p < 170 && getB p < 170

type Point2 = (Int,Int)
data InputLayer = InputLayer 
	{ height :: Int
	, width :: Int
	, dataMap :: Map Point2 Bool 
	} deriving Show

inputLayerCoords :: InputLayer -> [] Point2
inputLayerCoords (InputLayer h w _) = [(x,y) | y <- [0..h- 1], x <- [0..w- 1]]

makeSynapseCoords :: Int -> Int -> [[Point2]]
makeSynapseCoords width step = [[(x2,y2) | x2 <- [x..x+step-1], y2 <- [y..y+step-1]] | x <- [0,step..(width - 1)], y <- [0,step..(width - 1)]]


prettyPrintLayer :: InputLayer -> IO ()
prettyPrintLayer (InputLayer h w m) = let
		showActive x y = case (M.member (x,y) m, m M.! (x,y)) of
				(False, _) -> 'X'
				(True, True) -> '+'
				(True, False) -> '_'
		printLine h = putStrLn $ foldr (\x l -> (:) (showActive x h) l) [] [0..w - 1] 
	in mapM_ printLine [0..(h - 1)]

main = do
	SDL.init [InitEverything]
	testImage <- Image.load "letter_a.png"
	let
		width = surfaceGetWidth testImage
		height = width
		coords = [(x,y) | y <- [0..height - 1], x <- [0..width - 1]]
	pixels <- sequence $ Prelude.map (\(x,y) -> fmap (getPixelActive) $ getPixel x y testImage) coords 
	let
		inputLayer = InputLayer height width . M.fromList $ zip coords pixels
	--putStrLn . show $ map getRGBData pixels
	--putStrLn . show $ coords
	putStrLn . show $ inputLayer
	prettyPrintLayer inputLayer
	
	return ()
		
