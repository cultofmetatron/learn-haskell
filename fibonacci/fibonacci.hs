fibonacci 1 = 1
fibonacci 2 = 1
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)


fibonacci_iter n counter acc1 acc2 = if (counter == n) 
  then acc1 + acc2 
  else fibonacci_iter n (counter + 1) (acc1 + acc2) acc1

fibby 0 = 0
fibby 1 = 1
fibby 2 = 1
fibby x = fibonacci_iter x 3 1 1



