import Data.List (transpose)

mul :: Num a => [[a]] -> [[a]] -> [[a]] 
mul a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

divide :: (a -> Bool) -> [a] -> [[a]]
divide _ [] = []
divide p xs@(x:xs') | p x       = takeWhile p xs : divide (not . p) (dropWhile p xs)
                    | not $ p x = takeWhile (not . p) xs : divide p (dropWhile (not . p) xs)

compress :: Eq a => [a] -> [a]
compress []     = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

encode :: Ord a => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = (length $ x : takeWhile (==x) xs, x) : encode (dropWhile (==x) xs)                          

repli :: [a] -> Int -> [a]
repli xs n = concat $ map (replicate n) xs

change :: (Ord a, Num a) => a -> [[a]]
change n | n < 0     = []
         | n == 0    = [[]]
         | otherwise = concat [map (x:) $ change $ n - x | x <- coins] where
            coins = [2, 3, 7]

seqA :: Int -> Integer
seqA n = mySeq !! n where
    mySeq = 1:2:3:(zipWith3 (\x y z -> (-2)*x+y+z) mySeq (tail mySeq) (tail $ tail mySeq))

empDep          = [("Mike", "It"), ("Jan", "Sales")]
depCountry      = [("It", "Japan"), ("Sales", "USA")]
countryCurrency = [("Japan", "JPY"), ("USA", "USD")]
currencyRate    = [("JPY", 112), ("USD", 1)]

f :: String -> Maybe Int -- f возвращает страну по имени сотрудника
f emp = case lookup emp empDep of
          Nothing   -> Nothing
          Just dep  -> case lookup dep depCountry of
                         Nothing      -> Nothing
                         Just country -> case lookup country countryCurrency of
                                           Nothing       -> Nothing
                                           Just currency -> lookup currency currencyRate

fB emp = lookup' empDep emp >>= lookup' depCountry >>= lookup' countryCurrency
         >>= lookup' currencyRate where
           lookup' ps k = lookup k ps

fD emp = do dep <- lookup emp empDep
            country <- lookup dep depCountry
            currency <- lookup country countryCurrency
            lookup currency currencyRate
