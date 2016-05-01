{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.List (sort)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  instances <- readInput
  let slns = map solvePuzzle instances
  mapM_ printSolution slns

data Instance = Case Int String
data Solution = Sln Int String

readInput :: IO [Instance]
readInput = do
  numCases <- readLn
  cases <- replicateM numCases readInstance
  return $ zipWith Case [1..numCases] cases

readInstance :: IO String
readInstance = getLine

printSolution :: Solution -> IO ()
printSolution (Sln cn digits) =
  putStrLn $ "Case #" ++ show cn ++ ": " ++ digits

solvePuzzle :: Instance -> Solution
solvePuzzle (Case n ds) = Sln n number
  where number = getDigits ds

getDigits :: String -> String
getDigits = doPuz . buildMap

buildMap :: String -> M.Map Char Int
buildMap = M.fromListWith (+) . map (\x -> (x,1))

-- given a unique char, the total list of chars, and a map, return the number
-- of those removed from the map and the new map
findNumber :: Char -> String -> M.Map Char Int -> (Int, M.Map Char Int)
findNumber c english m = (n, m')
  where
    n = case M.lookup c m of
      Nothing -> 0
      Just count -> count
    m' = removeNStrings n english m

removeNStrings :: Int -> String -> M.Map Char Int -> M.Map Char Int
removeNStrings n chars m = foldl del m chars
  where
    del :: M.Map Char Int -> Char -> M.Map Char Int
    del m c = M.adjust (\x -> x - n) c m

numbers =
  [('0', 'Z', "ZERO"), ('2', 'W', "TWO"), ('4', 'U', "FOUR"), ('6', 'X', "SIX"), ('8','G',"EIGHT")
  ,('1', 'O', "ONE"), ('3', 'T', "THREE"), ('5', 'F', "FIVE"), ('7', 'S', "SEVEN")
  ,('9', 'I', "NINE")]

doPuz m = sort . fst $ foldl go ("",m) numbers
  where
    go (ds, m) (d, u, s) = (replicate n d ++ ds, m')
      where (n, m') = findNumber u s m 
