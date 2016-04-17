{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Monad (replicateM)
import Data.Sequence hiding (zipWith, replicateM)
import GHC.Exts (toList)

main :: IO ()
main = do
  instances <- readPuzzle
  let slns = map findSolution instances
  mapM_ printSolution slns

data Instance = Case Int String
data Solution = Sln Int String

readPuzzle :: IO [Instance]
readPuzzle = do
  n <- readLn
  cases <- replicateM n getLine
  return $ zipWith Case [1..n] cases

findSolution :: Instance -> Solution
findSolution (Case n inp) = Sln n result
  where result = do_puzzle inp

printSolution :: Solution -> IO ()
printSolution (Sln n result) =
  putStrLn $ "Case #" ++ show n ++ ": " ++ result

do_puzzle :: Ord a => [a] -> [a]
do_puzzle (h:t) = toList $ foldl go (singleton h) t
  where go s l = let x :< _ = viewl s in
                 if l >= x
                   then l <| s
                   else s |> l
