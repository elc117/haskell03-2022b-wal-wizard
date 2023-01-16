-- #1 --
add10toall :: [Int] -> [Int]
add10toall x = [x + 10 | x <- [0, 1, 2]]

-- #2 --
multN :: Int -> [Int] -> [Int]
multN n m = [n * m | n <- [5], m <- [1, 2, 3, 4, 5]]

-- #3 --
multN2 :: Int -> [Int] -> [Int]
multN2 x y = map (* 2) [1 .. 10]

-- #4 --
applyExpr :: [Int] -> [Int]
applyExpr x = [3 * x + 2 | x <- [1, 2, 3, 4, 5, 6, 7, 8, 9]]

-- #5 --
applyExprLB :: [Int] -> [Int]
applyExprLB lampFunc = map (\x -> 3 * x + 2) lampFunc

-- #6 --
addSuffix :: String -> [String] -> [String]
addSuffix name prefix = [name ++ prefix | name <- ["waliston", "fulano", "beltrano", "ciclano"], prefix <- ["@hotmail.com"]]

-- #7 --
selectgt5 :: [Int] -> [Int]
selectgt5 nums = [nums * 2 | nums <- [0, 1, 2, 3, 4, 5], nums > 5]

-- #8 --
sumOdds :: [Int] -> Int
sumOdds y = sum [x | x <- y, odd x]
-- #9 --

sumOdds' :: [Int] -> Int
sumOdds' y = foldl (+) 0 (filter (\x -> odd x) y)
-- #10 --
selectExpr :: [Int] -> [Int]
selectExpr n = [n * 2 | n <- [1 .. 10]]

-- #11 --
countShorts :: [String] -> Int
countShorts y = length [x | x <- y, length x < 5]

-- #12 --
calcExpr :: [Float] -> [Float]
calcExpr y = [x ^ 2 / 2 | x <- y, (x ^ 2 / 2) > 10]

-- #13 --
trSpaces :: String -> String
trSpaces y = [if x == ' ' then '-' else x | x <- y]

-- #14 --
selectSnd :: [(Int, Int)] -> [Int]
selectSnd y = [snd x | x <- y]

-- #15 --
dotProd :: [Int] -> [Int] -> Int
dotProd x1 y1 = sum [x * y | (x, y) <- zip x1 y1]

main = do
  print (sumOdds [1 .. 5])
  print (sumOdds' [1 .. 5])
  print (selectExpr [1 .. 100])
  print (countShorts ["zxcvqwe", "asd"])
  print (calcExpr [1.0 .. 10.0])
  print (trSpaces "asda asd as a")
  print (selectSnd [(0, 1), (1, 2), (3, 4)])
  print (dotProd [1, 1, 1, 1] [2, 2, 2, 2])