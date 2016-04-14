{-|-}
module Graphics.UI.GLCV.Event where

import Graphics.UI.GLCV.Key

-- | Event types
data Event = Init -- ^ Application initialization.
           | Term -- ^ Application termination.
           | GLInit -- ^ Application initialization after context creation.
           | GLTerm -- ^ Application initialization prior to context destruction.
           | Down Key -- ^ Key pressed.
           | Up Key -- ^ Key released.
           | Unicode Char -- ^ Keyboard input.
           | Motion Int Int -- ^ Mouse input.
           | Close -- ^ Window close.
           | Resize Int Int -- ^ Window resize.
           | Update -- ^ Update window (redraw).
           deriving Show
