module GLCV.Event where

import GLCV.Key

data Event = Init
           | Term
           | GLInit
           | GLTerm
           | Down Key
           | Up Key
           | Unicode Char
           | Motion Int Int
           | Close
           | Invoke
           | Resize Int Int
           | Update
           | Unknown
           deriving Show
