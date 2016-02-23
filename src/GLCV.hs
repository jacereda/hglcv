{-# LANGUAGE ForeignFunctionInterface #-}
module GLCV(run, quit, fullscreen, windowed, size, mouse) where
import Unsafe.Coerce
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Control.Monad
import Data.Char(chr)

import GLCV.Event

newtype Ev = Ev (Ptr Ev)

foreign import ccall "cvQuit" quit :: IO ()
foreign import ccall "cvFullscreen" fullscreen :: IO ()
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

pack :: CInt -> CInt -> (Int, Int)
pack x y = (fromIntegral x, fromIntegral y)

size :: IO (Int, Int)
size = ($!) liftM2 pack cvWidth cvHeight

mouse :: IO (Int, Int)
mouse = ($!) liftM2 pack cvMouseX cvMouseY

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

run :: String -> Int -> Int -> Int -> Int -> (Event -> IO ()) -> IO ()
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
          REInvoke -> wevent Invoke
          REResize -> Resize <$> evWidth ev <*> evHeight ev >>= wevent
          REUpdate -> wevent Update
          _ -> ret 0
  whandler <- wrap handler
  cvRun whandler
  freeHaskellFunPtr whandler
  return ()
