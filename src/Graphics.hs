{-# LANGUAGE RecordWildCards #-}
module Graphics where

import Gelatin
import Gelatin.SDL2
import Gelatin.FreeType2
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

import Tetris
import Network

data Resources = Resources { rsrcBackends      :: SDL2Backends
                           , rsrcAtlas         :: Atlas
                           , rsrcBlockRenderer :: Renderer2
                           , rsrcLastTime      :: Float 
                           }

blockExtant :: Float
blockExtant = 10

blockPicture :: ColorPicture ()
blockPicture = setGeometry $ triangles $ do
  tri (0, white) (V2 blockExtant 0, white) (V2 blockExtant blockExtant, white)
  tri (0, white) (V2 blockExtant blockExtant, white) (V2 0 blockExtant, white)

tetronimoColor :: Tetronimo -> V4 Float 
tetronimoColor t = fromMaybe white $ lookup (tetronimoShape t) table 
    where table  = zip [minBound .. maxBound :: TetronimoShape] colors
          colors = [ V4 0.0 1.0 1.0 1.0
                   , V4 0.0 0.0 1.0 1.0
                   , V4 1.0 0.5 0.0 1.0
                   , V4 1.0 1.0 0.0 1.0
                   , V4 0.0 1.0 0.0 1.0
                   , V4 0.5 0.0 0.5 1.0
                   , V4 1.0 0.0 0.0 1.0
                   ]

drawTetronimo :: Resources -> Tetronimo -> IO ()
drawTetronimo r t = forM_ (tetronimoBlocks t) $ \v -> do
  let color = tetronimoColor t
      pos   = blockExtant *^ fmap realToFrac v
  snd (rsrcBlockRenderer r) [moveV2 pos, multiplyV4 color]

drawBoard :: Resources -> Board -> IO Resources 
drawBoard r b = do
  mapM_ (drawTetronimo r) (boardStaticTetronimos b)  
  (drawTetronimo r) $ moveTetronimo (boardCurrentPosition b)
                                    (boardCurrentTetronimo b)
  let texture = backendV2V2 $ rsrcBackends r
  (text, _, atlas1) <-
    freetypeRenderer2 texture (rsrcAtlas r) white "TETRIS" 
  snd text [move 10 32]
  return $ r{rsrcAtlas=atlas1}
  
