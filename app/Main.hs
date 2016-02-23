module Main where

import GLCV
import GLCV.Key
import GLCV.Event

main :: IO ()
main = run "test" 0 0 640 480 on
  where on (Up Escape) = quit
        on (Up F) = fullscreen
        on (Up W) = windowed
        on Update = do
          s <- size
          m <- mouse
          putStrLn $ unwords ["size", show s, "mouse", show m]
        on e = print e
