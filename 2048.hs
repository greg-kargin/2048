module Main where
import Prelude hiding (Left, Right)
import Data.List
import System.IO
import Text.Printf
import System.Random
import Control.Monad.State

type Grid = [[Int]]
type Coord = (Int, Int)
data Move = Left | Right | Up | Down | Err

move :: Move -> Grid -> Grid
move Left  grid = map swipe grid
move Right grid = map (reverse.swipe.reverse) grid
move Up    grid = transpose $ move Left $ transpose grid
move Down  grid = transpose $ move Right $ transpose grid
move Err   grid = grid

getMove 'a' = Left
getMove 'd' = Right
getMove 'w' = Up
getMove 's' = Down
getMove _   = Err

swipe :: [Int] -> [Int]
swipe [] = []
swipe (x:[]) = [x]
swipe (0:xs) = swipe xs ++ [0]
swipe (x:0:xs) = (swipe (x:xs)) ++ [0]
swipe (x:y:xs) | x == y = ((x + y) : (swipe xs)) ++ [0]
               | otherwise = (x:(swipe (y:xs)))

newTile :: Grid -> IO Grid
newTile grid = do
  coord <- sample1 $ emptyCells grid
  value <- sample1 [2,4]
  return $ setTile coord value grid

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

printGrid :: Grid -> IO Grid
printGrid grid = do
  putStrLn ""
  mapM_ printRow grid
  putStrLn ""
  return grid

printRow :: [Int] -> IO ()
printRow r = do
  mapM_ (putStr.printf "%-6d") r
  putStrLn "\n"

gameLoop :: IO Grid -> IO ()
gameLoop grid = do
  g <- grid
  let e = emptyCells g
  if length e == 0
    then putStrLn "You've lost!"
    else do printGrid g
            arrowPressed <- getChar
            gameLoop $ newTile $ move (getMove arrowPressed) g


hFlushInput :: Handle -> IO ()
hFlushInput hdl = do
  r <- hReady hdl
  if r then do
    c <- hGetChar hdl
    hFlushInput hdl
  else
    return ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hFlushInput stdin

  gameLoop $ return [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
