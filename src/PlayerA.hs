module PlayerA where

import Value
import Cell
import Board
import Ship
import BoardUtil
import Control.Monad.Random


-- | This is the function stub you have to improve. Currently it plays the first
-- empty cell it finds.


-- The grid has the following coordinate system:
--
-- >  (i,j)
-- > + - - + - - + - - +
-- > | 0,0 | 0,1 | 0,2 |
-- > + - - + - - + - - +
-- > | 1,0 | 1,1 | 1,2 |
-- > + - - + - - + - - +
-- > | 2,0 | 2,1 | 2,2 |
-- > + - - + - - + - - +

play :: Board -> Cell
play (Board _ board) = playing board (Cell 0 0)
  where
    playing :: [Row] -> Cell -> Cell
    playing [] _             = error "No free cell in the board."
    --playing [] _    = playRow row cell
    playing (row: rows) cell = playRow row cell
      where
        playRow :: Row -> Cell -> Cell
        -- No free cell in this row
        playRow [] (Cell i _) = playing rows (Cell (i + 1) 0)
        playRow (nextCell: cells) c@(Cell i j)
          | nextCell == U    = c 
          | otherwise = playRow cells (Cell i (j + 1))

-- | You can provide your own starting board, just make sure it is valid.
startingBoard :: Monad m => m Board
startingBoard = do
  pure clusteredBoard

-- | This is a more advanced version of the previous function, allowing you to
-- use Monads. Use wisely.

{- | This functions basically takes a board and initially when the 
     board is empty it chooses a random cell and plays it. Then it identifies the hit cell and 
     takes it out in a list. Then with the help of another function, it checks for valid neighbours 
     to playit. If the neighbour is not valid, it returns an error message. Else it checks if the cell
     is attackable or not. If it finds an attackable valid cell, it plays that cell and then recursively 
     keeps on doing this.   
-}
mplay :: (MonadRandom m) => Board -> m Cell
mplay (board@ (Board size rows)) = do
    let boardCells = identifiedCells board          -- identifying the cells
        hits = filter (\(cell, value) -> value == X) boardCells  -- filtering out the cells which are already hit
    if null hits                                    -- if no hits
    then do
      coord <-  chooseRandom [(x, y) | y <- [0..size - 1], x <- [0..size -1] ]   -- choosing a random coordinate in board   
      if getCell (Cell (fst coord) (snd coord)) board == U   -- if the cell is undiscovered 
      then  return (Cell (fst coord) (snd coord))            -- get the coordinate of the cell
      else mplay board                                       -- if not U then choose random again
    
    else do
      result <- hitsearch hits                            -- calling the hitsearch function to get the hits
      return result
  where
    hitsearch :: (MonadRandom m) => [(Cell, Value)] -> m Cell
    hitsearch [] = error "No free cell in the board."
    hitsearch (h:hitsrest) = do                              
      let validNeighbors = filter (\c -> validCellFilter c && attackable c) $ neighbours (fst h)   -- filtering out the valid cells and attackable cells
          validCellFilter (Cell i j)                 -- setting valid cell
            | i < 0 || j < 0 || i >= size || j >= size = False   
            | otherwise = True
          attackable cell = getCell cell board == U    -- if undiscovered and attackable
      case validNeighbors of     -- setting two cases
        [] -> hitsearch hitsrest  -- when there is nothing to hit, try the rest hits
        (x:_) -> return x        -- if there is something to hit then hit it. 


