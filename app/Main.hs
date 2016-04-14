module Main where

import           Graphics.GL
import           Graphics.UI.GLCV
import           Graphics.UI.GLCV.Event
import           Graphics.UI.GLCV.Key

main :: IO ()
main = run "test" 0 0 640 480 on
  where on (Up Escape) = quit
        on (Up F) = fullscreen
        on (Up W) = windowed
        on (Resize w h) = do
          glViewport 0 0 (fromIntegral w) (fromIntegral h)
          glMatrixMode GL_PROJECTION
          glLoadIdentity
        on Update = do
          w <- width
          h <- height
          x <- mouseX
          y <- mouseY
          let r = (fromIntegral x / fromIntegral w)
          let g = (fromIntegral y / fromIntegral h)
          glClearColor r g 0 1
          glClear GL_COLOR_BUFFER_BIT
        on e = print e
