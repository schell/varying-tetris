module Tetris where

import Gelatin

-- | The shape of a tetris piece.
data TetronimoShape = I | J | L | O | S | T | Z | CustomTetronimoShape
                    deriving (Show, Eq, Ord, Enum, Bounded)

-- | A list of occupied spaces. 
data Tetronimo = Tetronimo { tetronimoShape  :: TetronimoShape
                           -- ^ The shape of the tetronimo.
                           , tetronimoBlocks :: [V2 Int] 
                           -- ^ The occupied spaces in the tetronimo.
                           }

-- | Given a tetronimo shape, returns a standard tetronimo.  
tetronimo :: TetronimoShape -> Tetronimo
tetronimo I = Tetronimo I [V2 0 0, V2 1 0, V2 2 0, V2 3 0] 
tetronimo J = Tetronimo J [V2 1 0, V2 1 1, V2 1 2, V2 0 1]
tetronimo L = Tetronimo L [V2 0 0, V2 0 1, V2 0 2, V2 1 2]
tetronimo O = Tetronimo O [V2 0 0, V2 1 0, V2 1 1, V2 0 1]
tetronimo S = Tetronimo S [V2 1 0, V2 2 0, V2 0 1, V2 1 1]
tetronimo T = Tetronimo T [V2 0 0, V2 1 0, V2 2 0, V2 1 1]
tetronimo Z = Tetronimo Z [V2 0 0, V2 1 0, V2 1 1, V2 2 1]
tetronimo _ = Tetronimo CustomTetronimoShape []

-- | Rotates a tetronimo clock-wise.
rotateTetronimoCW :: Tetronimo -> V2 Int -> Tetronimo
rotateTetronimoCW (Tetronimo s xs) (V2 x y) = Tetronimo s ys
  where ys = map op xs
        op = fmap round . demote . rot . promote . fmap realToFrac
        promote (V2 a b)   = V3 (V1 a) (V1 b) (V1 1)
        demote  (V3 (V1 a) (V1 b) _) = V2 a b
        rot v = back !*! rmat !*! there !*! v
        theta = pi / 2
        [nx, ny] = map ((* (-1)) . realToFrac) [x,y] :: [Float]
        [xx, yy] = map realToFrac [x,y] :: [Float]
        (sn,cs) = (sin theta, cos theta)
        rmat  = V3 (V3 cs    sn 0)
                   (V3 (-sn) cs 0)
                   (V3 0     0  1)
        there = V3 (V3 1     0  nx)
                   (V3 0     1  ny)
                   (V3 0     0  1 )
        back  = V3 (V3 1     0  xx)
                   (V3 0     1  yy)
                   (V3 0     0  1 )

-- | Rotates a tetronimo counter clock-wise.
rotateTetronimoCCW :: Tetronimo -> V2 Int -> Tetronimo
rotateTetronimoCCW t v =
  rotateTetronimoCW (rotateTetronimoCW (rotateTetronimoCW t v) v) v

-- | Determines if a tetronimo overlaps any in the list.
tetronimoOverlaps :: Tetronimo -> [Tetronimo] -> Bool
tetronimoOverlaps (Tetronimo _ xs) = any overlaps
  where overlaps (Tetronimo _ ys) = or [ x == y | x <- xs, y <- ys ]

-- | All the state needed to play a game of tetris. 
data Board = Board { boardCurrentTetronimo :: Tetronimo
                   -- ^ The current falling tetronimo.
                   , boardCurrentPosition  :: V2 Int
                   -- ^ The position of the current tetronimo.
                   , boardStaticTetronimos :: [Tetronimo]
                   -- ^ All the previously fallen tetronimos.
                   , boardNextTetronimos   :: [Tetronimo]
                   -- ^ All the next tetronimos. This should be an infinite list.
                   }
