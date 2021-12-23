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

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib a b counter = a + fastFib b (a + b) (counter - 1)

myFib n = fastFib 1 1 n

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
