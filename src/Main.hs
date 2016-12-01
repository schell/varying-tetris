{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
import SDL hiding (Event, Renderer)
import Gelatin.SDL2
import Gelatin.FreeType2
import Control.Varying
import Control.Concurrent    (threadDelay)
import Data.Functor.Identity (runIdentity)

import Paths_varying_tetris
import Input
import Network

blockPicture :: ColorPicture ()
blockPicture = setGeometry $ triangles $ do
  let extant = 10
  tri (0, white) (V2 extant 0, white)      (V2 extant extant, white)
  tri (0, white) (V2 extant extant, white) (V2 0 extant, white)

data Resources = Resources { rsrcBackends      :: SDL2Backends
                           , rsrcAtlas         :: Atlas
                           , rsrcBlockRenderer :: Renderer2
                           , rsrcLastTime      :: Float 
                           }

renderFrame :: Resources -> Frame -> IO Resources 
renderFrame rsrc@Resources{..} (Frame n) = do
  let texture = backendV2V2 rsrcBackends
  (text, _, atlas1) <-
    freetypeRenderer2 texture rsrcAtlas white $ "tetris-varying frame " ++ show n
  snd rsrcBlockRenderer [move 100 100]
  snd text  [move 10 32]
  return $ rsrc{rsrcAtlas=atlas1}

stepSplineMany :: Monad m => SplineT a b m c -> [a] -> a
               -> m (Either c (b, SplineT a b m c))
stepSplineMany n [] x = runSplineT n x
stepSplineMany n (x:xs) y = runSplineT n x >>= \case 
  Left c        -> return $ Left c
  Right (_, n1) -> stepSplineMany n1 xs y

loop :: Resources -> Spline Input Frame () -> IO ()
loop rsrc@Resources{..} net = do
  threadDelay 1
  newTime <- SDL.time
  let SDL2Backends color _ = rsrcBackends 
      t = newTime - rsrcLastTime
  events  <- eventsToInputs <$> getEvents color 
  clearWindow color
  case runIdentity $ stepSplineMany net events (InputTime t) of
    Left () -> do
      updateWindow color
      putStrLn "Thank you for playing, bye!"
    Right (frame, net1) -> do
      rsrc1 <- renderFrame rsrc frame
      updateWindow color
      loop rsrc1{rsrcLastTime=newTime} net1 

main :: IO ()
main = do
  backend     <- startupSDL2Backends 800 600 "varying-tetris" True 
  renderBlock <- snd <$> compilePicture (backendV2V4 backend) blockPicture
  fontName    <- getDataFileName "assets/ChicagoFLF.ttf"
  allocAtlas fontName (PixelSize 32 32) asciiChars >>= \case
    Nothing    ->
      putStrLn $ "Could not alloc an atlas for font " ++ show fontName
    Just atlas -> do
      t <- SDL.time 
      loop (Resources backend atlas renderBlock t) network 
