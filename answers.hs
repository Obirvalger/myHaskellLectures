import Data.List

reverse' = reverseA [] where
  reverseA acc []     = acc
  reverseA acc (x:xs) = reverseA (x:acc) xs 

-- Удвоить каждый четный элемент в списке
doubleEven = concat . map (\x -> if even x then [x,x] else [x])

-- Удвоить елементы стоящие на четной позиции с конца, утроить на нечетной
doubleTriple = reverse . triple . reverse where
  triple []     = []
  triple (x:xs) = x : x : x : double xs

  double []     = []
  double (x:xs) = x : x : triple xs
 
-- Граф задан числом вершин n и списком ребер xs
-- Функция принимает n и xs и возвращает точки сочленения
bridges :: Int -> [(Int, Int)] -> [Int]
bridges n xs = fnd $ map (\i -> isConnected (delete i [1..n]) $ rm xs i) [1..n]

fnd :: [Bool] -> [Int]
fnd = map fst . filter (not . snd) . zip [1..]

isConnected :: [Int] -> [(Int, Int)] -> Bool
isConnected vs xs = all (==vs) $ map (reachable xs) vs

reachable :: [(Int, Int)] -> Int -> [Int]
reachable xs x = stop $ iterate (f xs) [x] where
  f :: [(Int, Int)] -> [Int] -> [Int]
  f xs ys = union ys (ys >>= step xs)

  stop (xs:ys:xss) | sort xs == sort ys = sort xs
                   | otherwise          = stop (ys:xss)

step :: [(Int, Int)] -> Int -> [Int]
step xs x = concat $ map (f x) xs where
  f x (a,b) | x == a   = [b]
            | x == b   = [a]
            |otherwise = []

rm :: [(Int, Int)] -> Int -> [(Int, Int)]
rm xs x = filter (\(a,b) -> x /= a && x /= b) xs

-- примеры
n1 :: Int
n1 = 5

graph1 :: [(Int, Int)]
graph1 = [(1,2),(2,3),(1,3),(3,4),(3,5),(4,5)]
{-
 
 1    4
 |\3 /|
 | \/ |
 | /\ |
 |/  \|
 2    5

-}

n2 :: Int
n2 = 9

graph2 :: [(Int, Int)]
graph2 = [(1,2),(2,4),(1,3),(3,4),(3,5),(3,7),(5,6),(6,7),(4,8),(4,9),(8,9)]
{-     
       5__6
       |  |
 1____3|__|7
 |    |
 |    |  8
 |    | /|
 |____|/_|
 2    4  9

-}

n3 :: Int
n3 = 6

graph3 :: [(Int, Int)]
graph3 = [(1,2),(2,3),(3,4),(4,5),(5,6),(6,1)]
{-
    1___2
    /   \
  6/     \3
   \     /
    \___/
    5   4
-}
