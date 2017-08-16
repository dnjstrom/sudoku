module Main where

import Sudoku hiding (get)
import Control.Monad.State.Lazy
import Text.Read hiding (lift, get)
import System.Console.ANSI

type Move = (Int, Int, Entry Int)
type MySudoku =  Sudoku (Entry Int)

empty :: Sudoku (Entry Int)
empty = Sudoku $ replicate 9 $ replicate 9 Empty

shift :: [a] -> [a]
shift (x:xs) = xs ++ [x]
shift xs = xs

shiftN :: Int -> [a] -> [a]
shiftN n xs = iterate shift xs !! n

finished :: Sudoku (Entry Int)
finished = Sudoku $ map (uncurry shiftN) $ zip [1..9] (repeat $ map Entry [1..9])


getInt :: IO Int
getInt = read <$> getLine

askCoordinate :: String -> IO Int
askCoordinate question = do
  putStr question
  mCoord <- readMaybe <$> getLine
  case mCoord of
    Nothing -> askCoordinate question
    (Just coord) -> return coord

askEntry :: String -> IO (Entry Int)
askEntry question = do
  putStr question
  input <- readMaybe <$> getLine
  case input of
    Nothing -> return Empty
    (Just entry) -> return $ Entry entry

withColor :: Color -> IO a -> IO a
withColor color action = do
  setSGR [SetColor Foreground Vivid color]
  result <- action
  setSGR [Reset]
  return result

getMove :: IO Move
getMove = do
  putStrLn "Enter a move:"
  x <-  askCoordinate "X: "
  y <- askCoordinate "Y: "
  e <- askEntry "Value: "
  return (x, y, e)

printValid :: String -> Bool -> IO ()
printValid preface isValid = do
  putStr $ preface ++ " "
  let color = if isValid then Green else Red
  withColor color $ print isValid

play :: StateT MySudoku IO MySudoku
play = do
  liftIO clearScreen
  sudoku <- get
  liftIO $ print sudoku
  liftIO $ printValid "Sudoku is OK: " (valid sudoku)
  (x,y,n) <- liftIO getMove
  let sudoku' = set x y n sudoku
  if not (completed sudoku')
     then put sudoku' >> play
     else return sudoku'

main :: IO ()
main = do
  final <- evalStateT play finished
  withColor Green $ putStrLn "You've WON!" >> print final
