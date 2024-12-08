import Control.Applicative

-- TLDR; There's a decently readable version, and a one liner coming from the depths of the earth:
-- readFile "inputs/day2.txt" >>= print . length . filter (liftA2 (&&) (liftA2 (||) (all (>= 0)) (all (< 0))) (all (\d -> 0 < abs d && abs d <= 3))) . map (diff . map read . words) . lines

diff :: [Integer] -> [Integer]
diff l = zipWith (-) l $ drop 1 l

sameSign :: [Integer] -> Bool
sameSign l = all (>= 0) l || all (< 0) l

rightDiff :: [Integer] -> Bool
rightDiff = all (\d -> 0 < abs d && abs d <= 3)

main :: IO ()
main =
  do
    input <- readFile "inputs/day2.txt"
    -- Sensible version
    let reports :: [[Integer]] = filter (\i -> sameSign i && rightDiff i) $ map (diff . map read . words) $ lines input
    print $ length reports

    -- One liner madness (formatted for your sanity, unformatted version at the top of the file)
    readFile "inputs/day2.txt"
      >>= print
        . length
        . filter
          ( liftA2
              (&&)
              ( liftA2
                  (||)
                  (all (>= 0))
                  (all (< 0))
              )
              (all (\d -> 0 < abs d && abs d <= 3))
          )
        . map (diff . map read . words)
        . lines