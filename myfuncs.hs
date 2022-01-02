import Data.Char (toUpper)
myGdc a 0 = a
myGdc a b = myGdc b (a `mod` b)


myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (x:xs) = myDrop (n - 1) xs

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs

myFilter f [] = []
myFilter f (x:xs) = if f x then x : myFilter f xs else myFilter f xs

myRemove f [] = []
myRemove f (x:xs) = if f x then myRemove f xs else x : myRemove f xs

myProduct xs = foldl (*) 1 xs

myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f (f init x) xs

myFoldr f init [] = init
myFoldr f init (x:xs) = f x (myFoldr f init xs)

appendToStart :: a -> [a] -> [a]
appendToStart x xs = x : xs
appendToEnd :: a -> [a] -> [a]
appendToEnd x xs = xs ++ [x]

reverseL :: Foldable t => t a -> [a]
reverseL = foldl (\ acc left -> left : acc) []
reverseR :: Foldable t => t a -> [a]
reverseR = foldr (\ right acc -> acc ++ [right] ) []

myElem x xs = length (filter isX xs) > 0
    where isX = (\ y -> x == y)

isPalindrome word = word == reverse word

isBetterPalindrome word = isPalindrome letters
    where notSpace char = char /= ' '
          letters =  map toUpper (filter notSpace word)

harmonic n = foldr (+) 0 list
    where list = map (1 /) [1 .. n]          