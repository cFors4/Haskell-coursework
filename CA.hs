------------Q1

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

mapSecond :: (a -> a) -> [a] -> [a]
mapSecond f = zipWith ($) (cycle [id, f])

isValidISBN13 :: [Int] -> Bool
isValidISBN13 x 
   | x == [] || length (x) /= 13 = False
   | otherwise = last x == 10 - (sum (mapSecond (*3) (init x))) `mod` 10 `mod` 10


------------Q2

convMap = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
           (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"),
           (4,"IV"), (1,"I")]


intToRoman :: Int -> String
intToRoman x 
    | x == 0 = ""
    | x > 0 = y ++ intToRoman(x-z)
       where (z,y) = head $ filter ((<=x).fst) convMap
          

romanToInt :: String -> Int
romanToInt n
    |n == "" = 0
    |otherwise = if fst(head(c))>0 then fst(head(c)) + romanToInt(drop 2 n) else fst(head(d)) + romanToInt(drop 1 n)
    where a = filter ((==) (take 2 n).snd) convMap
          b = filter ((==) (take 1 n).snd) convMap
          c = if a == [] then [(0,"")] else a
          d = if b == [] then [(0,"")] else b


------------Q3
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


add :: Tree Int -> Int -> Tree Int
add Empty x = Node x Empty Empty
add (Node z left right) x 
  | z == x = Node z left right
  | z  < x = Node z left (add right x)
  | z  > x = Node z (add left x) right

buildTree :: [Int] -> Tree Int
buildTree [] = Empty
buildTree (x:xs) = create (Node x Empty Empty) xs
  where 
    create currentTree [] = currentTree
    create currentTree (x:xs) = create (add currentTree x) xs



flattenTree::Tree Int -> [Int]
flattenTree Empty = []
flattenTree (Node x left right)
  =flattenTree left ++ [x] ++ flattenTree right

sort::[Int] ->[Int]
sort [] = []
sort x = flattenTree $ buildTree x



------------Q4



hidden =["LAPTOP", "KEYBOARD", "BUGS", "DISKETTE", "UPGRADE", "MEMORY", "HARDWARE", "FLOPPY", "HARDDRIVE", "SOFTWARE"]

grid  = [["I", "U", "P", "G", "R", "A", "D", "E", "E", "P", "E", "Q"],
         ["Y", "T", "D", "Z", "M", "T", "Z", "V", "N", "R", "X", "S"],
         ["Y", "V", "C", "E", "C", "T", "I", "W", "A", "L", "Z", "R"],
         ["P", "C", "P", "G", "E", "R", "S", "W", "G", "C", "R", "E"],
         ["P", "G", "L", "U", "D", "V", "D", "U", "C", "F", "N", "S"],
         ["O", "N", "T", "D", "J", "R", "R", "W", "D", "F", "O", "Y"],
         ["L", "V", "R", "G", "A", "X", "A", "I", "Y", "F", "K", "Z"],
         ["F", "A", "U", "H", "B", "G", "S", "X", "T", "E", "L", "I"],
         ["H", "E", "G", "S", "P", "K", "H", "W", "Y", "P", "O", "C"],
         ["T", "E", "S", "Z", "E", "B", "A", "B", "I", "D", "K", "Y"],
         ["N", "Z", "W", "T", "U", "R", "O", "H", "O", "I", "P", "K"],
         ["M", "X", "T", "G", "E", "A", "D", "G", "A", "V", "L", "U"],
         ["T", "E", "S", "S", "R", "M", "E", "M", "O", "R", "Y", "O"],
         ["D", "I", "Q", "D", "T", "R", "O", "M", "T", "K", "S", "L"],
         ["I", "R", "C", "T", "L", "A", "P", "T", "O", "P", "O", "X"]]


transpose :: [[a]] -> [[a]]
transpose ([] : xss) = []
transpose xss
  = map head xss : transpose (map tail xss)


diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonals (map tail xss)))

found :: (Eq a) => [a] -> [a] -> Bool
found [] _ = True
found _ [] = False
found a@(x:a') (y:b) | x == y    = found a' b
                  | otherwise    = found a b

----------DIRECTIONAL SEARCHES
right :: [[[Char]]] -> String -> String
right [] _ = []
right (y:ys) find =
  if found find z then "Right" else right ys find
  where z = concat(y)

left :: [[[Char]]] -> String -> String
left [] _ = []
left (y:ys) find =
  if found find z then "Left" else left ys find
  where z = concat(reverse(y))


------TRANSPOSE GRID BEFORE
up :: [[[Char]]] -> String -> String
up [] _ = []
up (y:ys) find =
  if found find z then "Up" else up ys find
  where z = concat(reverse(y))

down :: [[[Char]]] -> String -> String
down [] _ = []
down (y:ys) find =
  if found find z then "Down" else down ys find
  where z = concat(y)

----
upRight :: [[[Char]]] -> String -> String
upRight [] _ = []
upRight (y:ys) find =
  if found find z then "UPRIGHT" else upRight ys find
  where z = concat(y)

downLeft :: [[[Char]]] -> String -> String
downLeft [] _ = []
downLeft (y:ys) find =
  if found find z then "DOWNLEFT" else downLeft ys find
  where z = concat(reverse(y))

upLeft :: [[[Char]]] -> String -> String
upLeft [] _ = []
upLeft (y:ys) find =
  if found find z then "UPLEFT" else upLeft ys find
  where z = concat(y)

downRight :: [[[Char]]] -> String -> String
downRight [] _ = []
downRight (y:ys) find =
  if found find z then "DOWNRIGHT" else downRight ys find
  where z = concat(reverse(y))


wordSearch :: [[[Char]]] -> [String] -> String
wordSearch _ [] = []
wordSearch grid (x:xs) = do
   let a = right grid x           
   let b = left grid x            
   let c = up (transpose(grid)) x   
   let d = down (transpose(grid)) x 
   let e = upRight (diagonals(grid)) x
   let f = downLeft (diagonals(grid)) x
   let g = upLeft (diagonals(reverse(grid))) x
   let h = downRight (diagonals(reverse(grid))) x
   show a ++ "" ++ b ++ "" ++ c ++ "" ++ d ++ "" ++ e ++ "" ++  f ++ "" ++ g ++ "" ++ h ++ "      " ++ x ++ "\n" ++ wordSearch grid xs
   



------------MAIN
main :: IO ()
main = do 
    let a = boolToString(isValidISBN13 [ 9, 7, 8, 0, 1, 3, 7, 0, 5, 3, 4, 6, 9 ])
        b = boolToString(isValidISBN13 [ 9, 7, 8, 0, 1, 3, 7, 0, 5, 3, 4, 6])
        c = boolToString(isValidISBN13 [ 9, 7, 8, 0, 1, 3, 7, 0, 5, 3, 4, 6, 10])
        g = wordSearch grid hidden
    
    putStrLn $ a ++ " " ++ b ++" " ++ c ++ "\n " ++ g ++ " "
