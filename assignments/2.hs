--Solution to assignment: http://www.cmi.ac.in/~spsuresh/teaching/prgh15/assignments/Assignment2.pdf

import Data.List(sortBy) --Problem 1
import Data.List(foldl') -- most are using this
import Data.List(sort)
import Data.List(nub)

--Problem 1
--TODO: Ask if the order needs to be maintained (no reply from TAs, hence ordering)
foo1 xs = (sort (nub (segments xs)))
segments xs = Data.List.sortBy cmpr (segments' xs)
  where cmpr a b --decreasing order of length
          | length a > length b  = LT
          | length a < length b  = GT
          | length a == length b = EQ
        segments' [] = [[]]
        segments' [x] = [[x]]
        segments' (x:(y:xs)) = (x:[]):[x:xs | xs <- restSeg, y == head xs] ++ restSeg -- "head" logic doesn't work if numbers are repeated
          where restSeg = segments' (y:xs)

--Problem 2
--Order is not required (mentioned in the question)
parts n
  | n <= 0 = [[]]
  | n == 1 = [[1]]
  | otherwise = [i:xs | i <- [1..n], xs <- parts (n-i), (null xs || i <= head xs) ]


-- Problem 3
llsg xs = maxCount
  where (_, maxCount) = Data.List.foldl' step ([], 0) xs
        step (ys, maxCount) x = if (curCount + 1 > maxCount)
                                then (x:curxs, curCount + 1)
                                else (x:curxs, maxCount)
          where (curCount, curxs) = getSteepyArray x ys 0 0
                getSteepyArray a [] length curSum = (length, [])
                getSteepyArray a (z:zs) length curSum = if (a >= (curSum+z))
                                                        then (length'+1, (z:zs'))
                                                        else (length, [])
                  where (length', zs') = getSteepyArray a zs length (curSum+z)

--Problem 4
isnext :: [Char] -> [Char] -> Bool
isnext [] [] = False
isnext s1 s2 = (firstCondition s1 s2) || (secondCondition s1 s2)
  where firstCondition [] "a" = True
        firstCondition [] _   = False
        firstCondition _  []  = False
        firstCondition (s1:s1s) (s2:s2s) = if s1 == 'b' && s2 == 'a'
                                           then firstCondition s1s s2s
                                           else False
        secondCondition :: [Char] -> [Char] -> Bool
        secondCondition _ [] = False
        secondCondition [] _ = False
        secondCondition s1 s2 = (isValid && curState == secondOrThird)
          where  (_, isValid, curState) = Data.List.foldl' step (s2, True, 1) s1
                 step:: ([Char], Bool, Integer) -> Char -> ([Char], Bool, Integer)
                 step ([], _, _) x = ([], False, invalid)
                 step (_, False, _) _ = ([], False, invalid)
                 step ((xs2:s2s), valid, curState) xs1
                   | curState == 1 = if xs1 /= xs2
                                     then if xs1 == 'a' && xs2 == 'b'
                                          then (s2s, valid, secondOrThird)
                                          else ([], False, invalid)
                                     else (s2s, valid, first)
                   | curState == 2 = if xs1 == 'b' && xs2 == 'a'
                                     then (s2s, valid, secondOrThird)
                                     else ([], False, first)
                 invalid = -1
                 first = 1
                 secondOrThird = 2 -- states representing sub-conditions of second condition

--problem 5
next :: [Char] -> [Char]
next [] = "a"
next s1 = if aPos /= 0
          then genRule2 s1
          -- string has only b's in which case just return n+1 a's
          else Data.List.foldl' (\as c -> ('a':as)) ['a'] s1
  where (s1Length, aPos) = Data.List.foldl' step1 (0,0) s1
        step1 (l, pos) cs1 = if (cs1 == 'a')
                             then (l+1, l+1)
                             else (l+1, pos)
        genRule2 s1 = reverse finalStr
                      where (_, finalStr) = Data.List.foldl' step2 (aPos, []) s1
                            step2 (pos, s2) cs1
                              | pos > 1   = ((pos-1), cs1:s2)
                              | pos == 1  = ((pos-1), 'b':s2)
                              | pos < 1   = (pos, 'a':s2) --we don't care for pos anymore

--Problem 6
abundant as = if finalCount > 1
              then True
              else False
  where (_, finalCount) = Data.List.foldl' step ('x', 0) as
        step (lastChar, count) curChar
          | lastChar == 'a' && curChar == 'b' = (curChar, count+1)
          | otherwise = (curChar, count)
              

--problem 7
abundants = (filter abundant (iterate next ""))
