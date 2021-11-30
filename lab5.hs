--Лабораторна робота №5
--студента групи КН-32 
--Каліти Вячеслава
--Варіант 13

--Мета:
--Ознайомитись з модульною органiзацiєю програм та засобами введення-
--виведення. Набути досвiду компiляцiї Haskell-програм.

-- Завдання
-- Реалiзувати та скомпiлювати одну з програм, розроблених у лабора-
-- торнiй роботi No 3 для Вашого варiанта з введенням даних: а) з клавiатури,
-- б) з файлу та виведенням результатiв: в) на екран, г) у файл.

import System.Directory.Internal.Prelude (getArgs)

import System.IO (IOMode (ReadMode), openFile, hIsEOF, hGetLine, hClose)

-- Функція видалення зi списку елементів з i-го по k-й включно
removeNtoM :: Int -> Int -> [a] -> [a]
removeNtoM n m list = take (n-1) list ++ drop m list

main :: IO()
main = do
    --a) з клавіатури
    putStrLn "enter 2 numbers of diapason and message"
    arg1<-readLn 
    arg2<-readLn
    arg3<-getLine 

    --б) з файлу
    handle <- openFile "input.txt" ReadMode
    str <- hGetLine handle
    let n1 = read str::Int
    str <- hGetLine handle
    let n2 = read str::Int
    n3 <- hGetLine handle
    hClose handle 
    
    --в) на екран
    putStrLn(removeNtoM arg1 arg2 arg3)
    -- writeFile "file.out" (removeNtoM src)

    --г) у файл
    writeFile "output.txt" (removeNtoM n1 n2 n3)

-- Tests:
-- *Main> :main
-- enter 2 numbers of diapason and message
-- 2
-- 6
-- i love banana
-- i banana

--Висновок
--Під час лабораторної роботи ми ознайомились з модульною органiзацiєю програм та засобами введення-виведення.
--Ми набули досвiду компiляцiї Haskell-програм, і навіть скористались командою :main інтерпритатора.
--Також ми мали змогу попрацювати з файлами та консоллю засобами мови Haskell.