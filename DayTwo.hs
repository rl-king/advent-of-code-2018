module DayTwo where

import Data.List


main :: IO ()
main = do
  i <- lines <$> readFile "dayTwo.txt"
  print $ part1 i


part1 :: [String] -> Int
part1 xs =
  (length $ elemIndices 2 x) * (length $ elemIndices 3 x)
  where x = concatMap (nub . map length <$> group . sort) xs


-- part2 xs =
--   map (filter ((==) 1 . length)) $
--   map (\x -> map ((\\) x) xs) xs




-- part2 xs =
--   transpose $
--   map (\str -> filter (\x -> length (elemIndices x str) > 1) str) $
--   transpose xs
