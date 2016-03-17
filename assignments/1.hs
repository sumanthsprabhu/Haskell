----Solution to assignment: http://www.cmi.ac.in/~spsuresh/teaching/prgh15/assignments/Assignment1.pdf

--Problem 1
isPerfect :: Integer -> Bool
isPerfect n
  | n <= 0        = error ("Only positive integers please!")
  --No odd numbers upto 10^300 are found to be perfect. So we can return False for odd numbers.
  | (n `mod` 2 /=0) && (n < lastOdd) = False 
  | otherwise     = if 2*n == (sumAllDivisors n 1) 
                    then True
                    else False
  where sumAllDivisors n m
          | n == m      = n
          | otherwise   = if divides n m == 0
                          then m + sumAllDivisors n (m+1)
                          else sumAllDivisors n (m+1)
          where divides n m = n `mod` m
        lastOdd         = 10^300

--Problem 2
nextPerfect :: Integer -> Integer
nextPerfect n
  | n <=0     = error ("Only positive integers please!")
  | otherwise = if isPerfect (n+1) == True
                then (n+1)
                else nextPerfect (n+1)

--Problem 3
partitioned :: [Int] -> Bool
partitioned []      = False --no element exists hence false
partitioned (x:[])  = True  --single element which is true by default
partitioned array   = if (isIncreasingArray array == True) then True else partitionHelper array 0 (length array)
  where partitionHelper array curIndex arrayLength
          | curIndex == arrayLength = False
          | otherwise               = ((leftCheck array (array!!curIndex) curIndex) && (rightCheck array (array!!curIndex) curIndex arrayLength)) || (partitionHelper array (curIndex+1) arrayLength)
        leftCheck :: [Int] -> Int -> Int -> Bool
        leftCheck array curElem index
          | index == 0  = True
          | otherwise   = ((array !! (index-1)) <= curElem) && (leftCheck array curElem (index-1))
        rightCheck :: [Int] -> Int -> Int ->  Int -> Bool
        rightCheck array curElem index arrayLength
          | index == (arrayLength-1) = True
          | otherwise                = ((array !! (index+1))  > curElem) && (rightCheck array curElem (index+1) arrayLength)
        isIncreasingArray :: [Int] -> Bool
        isIncreasingArray [] = True
        isIncreasingArray (x:[]) = True
        isIncreasingArray (x:y:ys) = (x <= y) && isIncreasingArray (y:ys)

--Problem 4
connected :: [[Char]]  -> Bool

connected []           = True
connected (x:[])       = True
connected (x:(y:(xs))) = if duplicate(x:y:xs)
                         then False
                         else (isDistanceOne x y 0 == True) && connected (y:(xs))
  where
    isDistanceOne :: [Char] -> [Char] -> Integer -> Bool
    isDistanceOne [] [] count         = if count == 1 then True else False
    isDistanceOne (a:as) (b:bs) count = if length (a:as) /= length (b:bs)
                                        then False
                                        else (if a /= b
                                              then (if count /= 0 then False else isDistanceOne as bs (count+1))
                                              else isDistanceOne as bs count)
    --checks if there are any duplicates in a list                              
    duplicate :: [[Char]] -> Bool
    duplicate []     = False
    duplicate (x:xs) = (x `elem` xs) || (duplicate xs)
