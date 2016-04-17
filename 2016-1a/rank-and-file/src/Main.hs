module Main where

import Control.Monad (replicateM, join, guard)
import qualified Data.Vector as V
import Data.List (sort)
import Control.Parallel.Strategies

main :: IO ()
main = do
  instances <- readInput
  --print instances
  let slns = parMap rseq solvePuzzle instances
  --print slns
  mapM_ printSolution slns

--                   C#  n   arrays
data Instance = Case Int Int [InpArr] deriving (Show)
--                   N Ranks Files Missing
data Solution = Sln Int InpArr deriving (Show)
type InpArr = [Int]

type Row = [Int]
type Grid = [Row]

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
    start = initPuzzle n arrs
    (Step _ ranks files [] True) = head $ finishSteps n start
    missing = getMissing ranks files

getMissing ranks files = filter (/=0) $ map abs $ zipWith (-) (join $ rotate ranks) (join files)

finishSteps :: Int -> Step -> [Step]
finishSteps n s = (foldl (.) id (replicate (2*n-1) (join . map takeStep))) $ [s]

data Step = Step { stepN :: Int
                 , sRanks :: Grid
                 , sFiles :: Grid
                 , sRest :: [InpArr]
                 , sUnknown :: Bool
              } deriving (Show)

initPuzzle :: Int -> [InpArr] -> Step
initPuzzle n arrs = Step n [h] [] t False
  where (h:t) = sort arrs

takeStep :: Step -> [Step]
takeStep s@(Step _ r f [] True) = if length r == length f then [s] else []
takeStep   (Step n r f [] False) = takeStep (Step n r f [mkUnknown n] True)
takeStep (Step n ranks files (next:rest) False) = do
  s <- [ Step n (ranks ++ [next]) files rest False
       ,  Step n (ranks ++ [mkUnknown n]) files (next:rest) True
       , Step n ranks (files ++ [next]) rest False
       , Step n ranks (files ++ [mkUnknown n]) (next:rest) True]
  guard $ checkConsistent s
  guard $ length (sRanks s) <= n
  guard $ length (sFiles s) <= n
  return s

takeStep (Step n ranks files (next:rest) True) = do
  s <- [ Step n (ranks ++ [next]) files rest True
       , Step n ranks (files ++ [next]) rest True]
  guard $ checkConsistent s
  guard $ length (sRanks s) <= n
  guard $ length (sFiles s) <= n
  return s

checkConsistent :: Step -> Bool
checkConsistent (Step _ r f _ _) = checkConsistent' r f

checkConsistent' :: Grid -> Grid -> Bool
checkConsistent' ranks files = and cells
  where
    lf = length files
    lr = length ranks
    ranks' = take lf $ rotate ranks
    files' = map (take lr) files
    cells = zipWith checkCell (join ranks') (join files')


-- Turns ranks into files
rotate :: Grid -> Grid
rotate ([]:_) = []
rotate rs = map head rs : rotate (map tail rs)

mkUnknown :: Int -> InpArr
mkUnknown n = replicate n 0

checkCell x y = x == y || x*y==0
