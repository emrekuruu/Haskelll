{-# LANGUAGE BlockArguments #-}
import System.IO
import Distribution.Simple.BuildTarget (resolveBuildTargets)
import System.Win32 (COORD(x))
import Distribution.Simple.Utils (xargs)

factorial :: Int->Int
factorial 0 = 1
factorial n = n * factorial(n-1)



add :: Int->Int->Int
add a b = a + b


recAdd :: Int->Int
redAdd :: (Eq a, Num a, Num p) => a -> p
redAdd 0 = 0
recAdd 1 = 1
recAdd a =
    if (a < 10) then
        a + recAdd(a-1)
    else
        0


divides :: Int -> Int -> Bool
divides a b | rem b a == 0 = True
            | otherwise = False


lists :: [Int]->Int->[Int]
lists [] times = []
lists list times =  result
  where result = [times*x | x <- list, x < 100]



ldf k n | divides k n = k
        | k*2 > n = n
        | otherwise = ldf (k+1) n


removeFst :: Integral a=>a->[a]->[a]
removeFst a [] = []
removeFst a (x:xs) | a == x = xs
                   | otherwise = x:removeFst a xs



minNumber :: Integer->Integer->Integer
minNumber x y | x <= y = x
              | otherwise = y



mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)


maxNumber :: Integer->Integer->Integer
maxNumber x y | x >= y = x
              | otherwise = y


maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

maxElement :: [[Int]]->Int
maxElement [] = 0
maxElement a = maxInt(map(maxInt)a)

count :: Char->String->Int
count c [] = 0
count c (x:xs) | c == x = 1 + count c xs
               | otherwise = count c xs

countC :: Char->Int
countC c = c `count` "aabbaac"

takeOdds :: [Int]->[Int]
takeOdds [] = []
takeOdds (x:xs) | (rem x 2) == 0 = takeOdds xs
                | otherwise = x:takeOdds xs


copy :: Int -> Char -> String
copy 0 c = []
copy n c = c : (copy (n-1) c)

blowup :: String->String
blowup [] = []
blowup x = blowup' x 1

blowup' :: String->Int->String
blowup' [] n = []
blowup' (x:xs) n = copy n x ++ (blowup' xs (n+1))


lengths:: [[Int]]->[Int]
lengths x = map length x  


reverse':: [Int]->[Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


countD :: Eq a => [a]->[a]
countD [] = []
countD (x:xs) = x:countD(filter (isNotEqual x) xs)


isNotEqual :: Eq a => a->a->Bool
isNotEqual x y | x == y  = False
               | otherwise = True


 

