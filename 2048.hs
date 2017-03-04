module Main where
import System.IO
import System.Random
import Control.Monad.State

type Grid = [[Int]]
type Coord = (Int, Int)
data Move = Left | Right | Up | Down

move :: Move -> Grid -> Maybe Grid
move = undefined

newTile :: Grid -> IO (Maybe Grid)
newTile grid | emptyCells grid == [] = return Nothing
             | otherwise = do
                 coord <- sample1 $ emptyCells grid
                 value <- sample1 [2,4]
                 return $ Just $ setTile coord value grid

sample1 :: [b] -> IO b
sample1 xs = do
  let l = length xs - 1
  idx <- randomRIO (0,l)
  return $ xs !! idx

setTile :: Coord -> Int -> Grid -> Grid
setTile (x,y) v grid = yL ++ [rowEdited] ++ yG
  where yL = take y grid
        yG = drop (y+1) grid
        row = grid !! y
        rowEdited = take x row ++ [v] ++ drop (x+1) row


emptyCells :: Grid -> [Coord]
emptyCells grid = do
  (y, row) <- zip [0..] grid
  (x, tile) <- zip [0..] row
  guard $ tile == 0
  return $ (x,y)

getTile :: Coord -> Grid -> Int
getTile (x, y) grid = grid !! y !! x

main :: IO ()
main = undefined
