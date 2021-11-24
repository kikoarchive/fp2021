import Distribution.Simple.Utils (xargs)
import Data.List ()
-- Лабораторна робота №3
-- студента групи КН-32
-- Каліти Вячеслава
-- Варіант №13

-- Мета: Набути досвiду визначення та використання функцiй вищого порядку.
--Завдання 1.13. Знайти останнiй елемент списку.
--a)
listLast :: [a] -> a
listLast [x] = x
listLast (_:xs) = listLast xs
listLast [] = error "empty list"

--б)
listLast_:: [a] -> a
listLast_ = last 

--Test:
--Main> listLast_[1,2,3,4,5]
--5
--Main> listLast [1,2,3,4,5]
--5

--Завдання 2.13. Видалити зi списку елементи з i-го по k-й включно, напр. при i=2 та k=4: "asdfghj"⇒ "aghj".
--а)
myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop 0 x = x
myDrop n (x:xs) = myDrop (n-1) xs
myDrop _ _ = []

myTake :: (Ord t, Num t) => t -> [a] -> [a]
myTake n _ | n <= 0     = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n-1) xs

dropNtoM :: Int -> Int -> [a] -> [a]
dropNtoM n m xs = myTake (n-1) xs ++ myDrop m xs

--б)
removeNtoM :: Int -> Int -> [a] -> [a]
removeNtoM n m list = take (n-1) list ++ drop m list

--Test:
--Main> dropNtoM 2 4 "asdfghj"
--"aghj"
--Main> removeNtoM 2 4 "asdfghj"
--"aghj"

--Висновок: У ході даної лабораторної роботи було визначено декілька функцій вищого порядку з рекурсією
--та без, що допомогло ознайомитися зі способами визначення функцій вищого порядку Haskell із
--застосуванням та без вбудованих функцій.

