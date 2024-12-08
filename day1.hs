import Data.List

main :: IO ()
main = do
  input <- readFile "inputs/day1.txt"
  let locations = words input
  let ids :: [Int] = map read locations

  let left = sort $ map snd $ filter (even . fst) $ zip [0 ..] ids
  let right = sort $ map snd $ filter (odd . fst) $ zip [0 ..] ids

  let distance = foldl f 0 (zip left right) where f a (l, r) = a + abs (l - r)
  let similarityScore = sum $ map (\a -> a * length (filter (a ==) right)) left

  print distance
  print similarityScore