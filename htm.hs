
module HTM (--export stuff!
           ) where

import Data.Map (Map)
import qualified Data.Map as M

type Active                             = Bool
type Strength                           = Float
type Connection                         = (Active, Strength)
type Coord                              = (Int, Int, Int) -- Level, X, Y
type Threshold                          = Float

{-- Cells have the coordinates of *incoming* connections.  A cell
    by itself does not know where it is
--}
data Cell = Cell {cellDendrites         :: Map Coord Connection}

emptyCell = Cell {cellDendrites         = M.empty}

data Column = Column {colCoord          :: Coord
                     ,colCells          :: [Cell]
                     {--Column dendrites are feed-forward from
                        the lower level so for now they have
                        no concept of specific dendrite locations
                        Update: may not true - maybe use a Map
                                just like Cells
                     --}
                     ,colDendrites      :: Map Coord Connection
                     }

emptyColumn = Column {colCoord          = (0,0,0)
                     ,colCells          = []
                     ,colDendrites      = M.empty
                     }

data Region = Region {regCols           :: [Column]
                     ,regMinThreshold   :: Float
                      -- what else?
                     }

emptyRegion = Region {regCols           = []
                     ,regMinThreshold   = 0.5
                      }

{--
TODO: try and whack the calls to flip everywhere and embrace
      partial application
--}

isConnected :: Connection -> Threshold -> Bool
isConnected (active, strength) t = active && strength >= t

isCellActive :: Cell -> Threshold -> Bool
isCellActive c t = 
                let connections = (M.elems (cellDendrites c))
                in any (flip isConnected t) connections

isColActive :: Column -> Threshold -> Bool
isColActive c t = 
                let activeFeedForward = any (flip isConnected t) (M.elems (colDendrites c))
                    activeCell        = any (flip isCellActive t) (colCells c)
                in activeFeedForward || activeCell

-- this could turn into a more heavy-weight
-- function involving all of the regions
getOutput :: Region -> [Column]
getOutput reg = let cols        = (regCols reg)
                    threshold   = (regMinThreshold reg)
                in filter (flip isColActive threshold) cols

createCell :: Map Coord Connection -> Cell
createCell m = Cell { cellDendrites = m }

createColumn :: Coord -> [Cell] -> Map Coord Connection -> Column
createColumn coord cells proximalDendrites = 
        Column {colCoord        = coord
               ,colCells        = cells
               ,colDendrites    = proximalDendrites
               }

createRegion :: (Int, Int, Int) -> Threshold -> Region
createRegion (xd, yd, lvl) threshold = 
        let coords      = [(x, y, lvl) | x <- [0..xd], y <- [0..yd]]
            cells       = [] -- TODO
        in Region {regCols = map (\coord -> createColumn coord cells M.empty) coords
                  ,regMinThreshold = threshold
                  }

main :: IO ()
main = let cell1 = createCell $ M.fromList [((0,0,0), (True, 0.4))] 
           cell2 = createCell $ M.fromList [((0,5,5), (True, 0.4)), ((0, 10, 10), (True, 0.6))]
--           col1 = createColumn (0,0,0) [cell1, cell2], colDendrites = [(True, 0.3), (False, 0.7), (True, 0.5)]}
           region1 = createRegion (15, 15, 0) 0.4
           region2 = createRegion (7, 7, 1) 0.5
       in print "hi"