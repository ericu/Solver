module Lib
    ( test
    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (partition)

test :: IO ()
test = do
  putStrLn "Called test"
  putStrLn $ show initialBoard

data Coord = Coord { col :: Int, row :: Int } deriving (Show, Eq, Ord)
type Board = Map Coord (Set Coord)
type Unit = Set Coord

allPossibleCoordsFor :: Board -> Int -> Set Coord
allPossibleCoordsFor b n =
  let f coord possibilities output = if n `Set.member` possibilities
                                     then output `Set.insert` coord
                                     else output
  in Map.foldlWithKey f Set.empty b

allPossibleCoordsInUFor :: Board -> Unit -> Int -> Set Coord
allPossibleCoordsInUFor b u n =
  let cs = allPossibleCoordsFor b n
  in Set.filter (flip Set.member $ u) cs

-- This is the inner part of the interior check.  If in a unit a number appears
-- only in a single coord, then that coord can't hold anything but that number.
clearSolvedInUnitForN :: Int -> Board -> Unit -> Board
clearSolvedInUnitForN n b u =
  let cs = allPossibleCoordsInUFor b u n
  in if size cs == 1
     then
       let [c] = Set.toList cs
       in Map.insert c (Set.singleton n) b
     else b

clearSolvedForN :: Board -> Int -> Board
clearSolvedForN b i = Set.foldl (clearSolvedInUnitForN n) b allUnits

clearSolved :: Board -> Board
clearSolved b = foldl (clearSolvedForN b) [0..8]

clearSolvedIfMoreThan :: Board -> Int -> Board
clearSolvedIfMoreThan b count =
  let b' = clearSolved b
      count' = countSolved b'
  in if count' > count
     then clearSolvedIfMoreThan b count'
     else b

clearSolvedUntilStable :: Board -> Board
clearSolvedUntilStable b =
  let count = countSolved b
  in clearSolvedIfMoreThan b count

{-
clearSolvedUntilStable :: Board -> Board
clearSolvedUntilStable b =
  let initial = countSolved b
      b' = clearSolved b
      current = countSolved b'
  in if current > initial
     then clearSolvedUntilStable b'
     else b
 -}

countSolved :: Board -> Int
countSolved b =
  let solved = filter (\s -> size s == 1) (elems b)
  in length solved

allRows :: Set Unit
allRows = Set.fromList [Set.fromList [Coord c r | c <- [0..8]] | r <- [0..8]]
allCols :: Set Unit
allCols = Set.fromList [Set.fromList [Coord c r | r <- [0..8]] | c <- [0..8]]
allBoxes :: Set Unit
allBoxes = Set.empty -- TODO

allUnits :: Set Unit
allUnits = allRows `Set.union` allCols `Set.union` allBoxes

initialBoard :: Board
initialBoard =
  let blank = Set.fromList [0..8]
      allCoords = [Coord c r | c <- [0..8], r <- [0..8]]
  in Map.fromList $ zip allCoords $ repeat blank

-- TODO: Pretty board display
dumpBoard :: Board -> IO ()
dumpBoard b = do
  let allCoords = [Coord c r | c <- [0..8], r <- [0..8]]
      kvs = map (\c -> (show c) ++ ": " ++ (show b Map.! c) ++ "\n") allCoords
  putStrLn kvs
  
