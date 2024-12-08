import Data.List

main :: IO ()
main = do
  input <- readFile "inputs/day1.txt"
  let locations = words input
  let ids :: [Integer] = map read locations

  let left = sort $ map snd $ filter (even . fst) (zip [0 ..] ids)
  let right = sort $ map snd $ filter (odd . fst) (zip [0 ..] ids)

  let answer = foldl f 0 (zip left right) where f a (l, r) = a + abs (l - r)

  print answer