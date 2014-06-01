import Data.Time

physical d = sin $ 2 * pi * (fromIntegral d) / 23
emotional d = sin $ 2 * pi * (fromIntegral d) / 28
intellectual d = sin $ 2 * pi * (fromIntegral d) / 33

ap f d = let [x, y] = map f [d, d + 1] in (x, if x < y then '+' else '-')

main = do
  now <- getCurrentTime
  let days = diffDays (utctDay now) (fromGregorian 1988 1 15)
  putStr "Phy "
  print $ ap physical days
  putStr "Emo "
  print $ ap emotional days
  putStr "Int "
  print $ ap intellectual days
