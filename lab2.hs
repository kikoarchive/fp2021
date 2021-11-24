import Distribution.Simple.Utils (xargs)
import Data.List ()
-- Лабораторна робота №2
-- студента групи КН-32
-- Каліти Вячеслава
-- Варіант №13

-- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму
-- зiставлення зi зразком i роботи з кортежами та списками.

-- Завдання 1.13 Видалити кожен n-й елемент списку, напр. при n=2: "1234590"⇒"1350"
-- а)
dropEveryNth :: Int -> [a] -> [a]
dropEveryNth = recur 1
    where recur _ _ []     = []
          recur i n (x:xs) = if i == n
            then recur 1 n xs
            else x:recur (i+1) n xs

--б)
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

--Test:
--Main> dropEvery 2 "1234560"
--"1350"
--Main> dropEveryNth 2 "1234560"
--"1350"

--Завдання 2.13 Знайти усi простi числа в указаному дiапазонi.
--а)

factors :: Int -> [Int] 
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> Int -> [Int]
primes n k= [x | x <- [n..k], prime x]

primesT :: Integral a => [a]
primesT = primesTME               --

primesR :: Integral a => a -> a -> [a]
primesR a b = takeWhile (<= b) $ dropWhile (< a) primes