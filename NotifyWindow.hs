module NotifyWindow (
  drawMessage
  ) where

import qualified Data.Map as Map
import Graphics.X11.Xlib
import Data.Bits
import Data.Int
import Data.Word
import Data.List
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Debug.Trace

bgColor1 = "#2a2b2a"
bgColor2 = "#383f51"
wwidth = 300
wheight = 150

drawMessage :: String -> String -> IO ()
drawMessage title body = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      scr = defaultScreenOfDisplay dpy
  rootw <- rootWindow dpy dflt
  win <- mkUnmanagedWindow dpy scr rootw 0 0 wwidth wheight
  setTextProperty dpy win "mNotify" wM_NAME
  mapWindow dpy win
  drawBg dpy win
  printTitle title dpy win
  printBody body dpy win
  sync dpy False
  threadDelay (30 * 100000)
  destroyWindow dpy win
  sync dpy False

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
 let colormap = defaultColormap dpy (defaultScreen dpy)
 (approx,real) <- allocNamedColor dpy colormap color
 return $ color_pixel approx

fillRect :: Display -> Window -> String -> (Int32, Int32) -> (Word32,Word32) -> IO ()
fillRect dpy win color (x,y) (width,height) = do 
  fgcolor <- initColor dpy color
  gc <- createGC dpy win
  setForeground dpy gc fgcolor
  fillRectangle dpy win gc x y width height
  freeGC dpy gc

drawBg :: Display -> Window -> IO ()
drawBg dpy win = do
  fillRect dpy win bgColor2 (0,0) (wwidth,wheight)
  fillRect dpy win bgColor1 (2,2) (wwidth-4,wheight-4)

printText :: String -> Int32 -> Int32 -> String -> (Display -> Window -> IO ())
printText text x y color dpy win = do
  gc <- createGC dpy win
  let offset = x
      valign = y
  fgcolor <- initColor dpy color
  bgcolor <- initColor dpy bgColor1
  setForeground dpy gc fgcolor
  setBackground dpy gc bgcolor
  fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
  let wordList = words text
      wordWidths = map (toInteger . (+2) . textWidth fontStruc) wordList
      wordTokens = zip wordWidths wordList
      (_,asc,desc,_) = textExtents fontStruc text
      drawWrappedText (lineNumber,text) = drawImageString dpy win gc offset verticalOffset text
        where verticalOffset = (desc+asc+3)*(fromIntegral lineNumber) + valign
  let lines =  wordwrap wordTokens $ fromIntegral (wwidth - 50)
  mapM drawWrappedText $ zip [1..(length lines)] lines
  freeGC dpy gc
  freeFont dpy fontStruc 

wordwrap :: [(Integer, String)] -> Integer -> [String]
wordwrap tokens limit = lines
  where subsentences = scanl1 (\(ax,ay) (bx,by) -> (ax+bx, by) ) tokens
        groupedTokens = groupBy (\(a,_) (b,_) -> (==) (quot a limit ) (quot b limit)) subsentences 
        lines = map (unwords . (map snd)) groupedTokens

printTitle :: String -> Display -> Window -> IO ()
printTitle text = printText text 10 20 "lightblue"

printBody :: String -> Display -> Window -> IO ()
printBody text = printText text 10 50 "white"

mkUnmanagedWindow :: Display
 -> Screen
 -> Window
 -> Position
 -> Position
 -> Dimension
 -> Dimension
 -> IO Window

mkUnmanagedWindow dpy scr rw x y w h = do
 let visual = defaultVisualOfScreen scr
     attrmask = cWOverrideRedirect .|. cWBorderPixel .|. cWBackPixel
 win <- allocaSetWindowAttributes $ \attributes -> do
     set_override_redirect attributes True
     set_background_pixel attributes $ whitePixel dpy (defaultScreen dpy)
     set_border_pixel attributes $ blackPixel dpy (defaultScreen dpy)
     createWindow dpy rw x y w h 1 (defaultDepthOfScreen scr) inputOutput visual attrmask attributes
 return win
