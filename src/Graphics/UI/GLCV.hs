{-|-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.UI.GLCV( run
                       , quit
                       , fullscreen
                       , windowed
                       , size
                       , width
                       , height
                       , mouse
                       , mouseX
                       , mouseY
                       ) where

import           Data.Char              (chr)
import           Foreign.C
import           Foreign.C.Types
import           Foreign.Ptr
import           Graphics.UI.GLCV.Event
import           Unsafe.Coerce

newtype Ev = Ev (Ptr Ev)

-- | Terminate event loop.
foreign import ccall "cvQuit" quit :: IO ()
-- | Enter fullscreen mode.
foreign import ccall "cvFullscreen" fullscreen :: IO ()
-- | Enter windowed mode.
foreign import ccall "cvWindowed" windowed :: IO ()
foreign import ccall "cvWidth" cvWidth :: IO CInt
foreign import ccall "cvHeight" cvHeight :: IO CInt
foreign import ccall "cvMouseX" cvMouseX :: IO CInt
foreign import ccall "cvMouseY" cvMouseY :: IO CInt
foreign import ccall "cvRun" cvRun :: FunPtr (Ev -> IO CIntPtr) -> IO CInt

foreign import ccall "evType" evType' :: Ev -> IO CInt
foreign import ccall "evX" evX' :: Ev -> IO CInt
foreign import ccall "evY" evY' :: Ev -> IO CInt
foreign import ccall "evWidth" evWidth' :: Ev -> IO CInt
foreign import ccall "evHeight" evHeight' :: Ev -> IO CInt
foreign import ccall "evWhich" evWhich' :: Ev -> IO CInt
foreign import ccall "evUnicode" evUnicode' :: Ev -> IO CInt

wrapped :: (Ev -> IO CInt) -> Ev -> IO Int
wrapped f arg = fmap fromIntegral (f arg)

evType, evX, evY, evWidth, evHeight, evWhich, evUnicode :: Ev -> IO Int
evType = wrapped evType'
evX = wrapped evX'
evY = wrapped evY'
evWidth = wrapped evWidth'
evHeight = wrapped evHeight'
evWhich = wrapped evWhich'
evUnicode = wrapped evUnicode'


width, height, mouseX, mouseY :: IO Int
-- | Current window width.
width = fromIntegral <$> cvWidth
-- | Current window height.
height = fromIntegral <$> cvHeight
-- | Current mouse X coordinate relative to window origin.
mouseX = fromIntegral <$> cvMouseX
-- | Current mouse Y coordinate relative to window origin.
mouseY = fromIntegral <$> cvMouseY

pack :: Int -> Int -> (Int, Int)
pack a b = seq a $ seq b (a,b)

size, mouse :: IO (Int, Int)
-- | Current window size.
size = pack <$> width <*> height
-- | Current mouse coordinate relative to window origin.
mouse = pack <$> mouseX <*> mouseY

foreign import ccall "wrapper"
  wrap :: (Ev -> IO CIntPtr) -> IO (FunPtr (Ev -> IO CIntPtr))

data RawEvent = RNone
              | RQName
              | RQLogger
              | RQXPos
              | RQYPos
              | RQWidth
              | RQHeight
              | REInit
              | RETerm
              | REGLInit
              | REGLTerm
              | REDown
              | REUp
              | REUnicode
              | REMotion
              | REClose
              | REInvoke
              | REResize
              | REUpdate
              | REUnknown
              | RInternalShowKeyboard
              | RInternalHideKeyboard
              | RInternalSetCursor
              | RInternalDefaultCursor
              | RInternalFullScreen
              | RInternalWindowed
              | RInternalQuit
              deriving (Show, Enum)

-- | Run the application loop.
run :: String -- ^ Window name.
    -> Int -- ^ Initial horizontal window coordinate.
    -> Int -- ^ Initial vertical window coordinate.
    -> Int -- ^ Initial window width.
    -> Int -- ^ Initial window height.
    -> (Event -> IO ()) -- ^ Event handler function.
    -> IO ()
run name x y w h on  = do
  let handler :: Ev -> IO CIntPtr
      handler ev = do
        et <- evType ev
        let ret = return . CIntPtr . fromIntegral
            wevent e = on e >> ret 0
        case toEnum $ fromEnum et of
          RQName -> withCString name $ \s -> return (unsafeCoerce s)
          RQXPos -> ret x
          RQYPos -> ret y
          RQWidth -> ret w
          RQHeight -> ret h
          REInit -> wevent Init
          RETerm -> wevent Term
          REGLInit -> wevent GLInit
          REGLTerm -> wevent GLTerm
          REDown -> evWhich ev >>= wevent . Down . toEnum
          REUp -> evWhich ev >>= wevent . Up . toEnum
          REUnicode -> evUnicode ev >>= wevent . Unicode . chr
          REMotion -> Motion <$> evX ev <*> evY ev >>= wevent
          REClose -> wevent Close
--          REInvoke -> Invoke <$> evMethod ev >>= wevent
          REResize -> Resize <$> evWidth ev <*> evHeight ev >>= wevent
          REUpdate -> wevent Update
          _ -> ret 0
  whandler <- wrap handler
  _ <- cvRun whandler
  freeHaskellFunPtr whandler
  return ()
