module DayOne where


main :: IO ()
main = do
  i <- lines <$> readFile "dayOne.txt"
  print $ part2 (read . f <$> i)
  where f ('+':xs) = xs
        f xs = xs


part1 :: [Int] -> Int
part1 =
  sum


part2 :: [Int] -> Maybe Int
part2 =
  f (Nothing, []) . scanl1 (+) . cycle
  where f (Just x, _) _ = Just x
        f (_, acc) (x:xs)
          | x `elem` acc = f (Just x, acc) xs
          | otherwise = f (Nothing, x:acc) xs
