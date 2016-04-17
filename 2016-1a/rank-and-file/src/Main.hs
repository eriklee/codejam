module Main where

import Control.Monad (join, replicateM)
import Data.List (sort)

main :: IO ()
main = do
  instances <- readInput
  let slns = map solvePuzzle instances
  mapM_ printSolution slns

--                   C#  n   arrays
data Instance = Case Int Int [InpArr] deriving (Show)
--                   N Ranks Files Missing
data Solution = Sln Int InpArr deriving (Show)
type InpArr = [Int]

readInput :: IO [Instance]
readInput = do
  numCases <- readLn
  cases <- replicateM numCases readInstance
  return $ zipWith (\c (n, arrs) -> Case c n arrs) [1..numCases] cases

readInstance :: IO (Int, [InpArr])
readInstance = do
  n <- readLn
  arrs <- replicateM (2 * n - 1) readArr
  return (n, arrs)
  
readArr :: IO InpArr
readArr = do
  ln <- getLine
  return $ map read . words $ ln

printSolution :: Solution -> IO ()
printSolution (Sln cn missing) =
  putStrLn $ "Case #" ++ show cn ++ ": " ++ (unwords $ map show missing)

solvePuzzle :: Instance -> Solution
solvePuzzle (Case cn n arrs) = Sln cn missing
  where
    start = rle . sort . join $ arrs
    missing = sort . map fst . filter (odd . snd) $ start

rle :: Eq a => [a] -> [(a,Int)]
rle [] = []
rle (x:xs) = (x, length (takeWhile (==x) (x:xs))) : rle (dropWhile (==x) xs)
