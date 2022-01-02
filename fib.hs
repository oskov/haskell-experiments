
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib a b counter = a + fastFib b (a + b) (counter - 1)

myFib n = fastFib 1 1 n

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)