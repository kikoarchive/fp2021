import Language.Haskell.TH (Type(ConT))
import Control.Concurrent (tryTakeMVar)
--Лабораторна робота №4
--студента групи КН-32 
--Каліти Вячеслава
--Варіант 13s

--Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

--Записник. У записнику зберiгається iнформацiя про знайомих: телефон
--(iм’я, телефон — один, або кiлька), нагадування про день народження
-- (iм’я, дата — день та мiсяць), зустрiчi (дата та час, мiсце, опис —
-- тема зустрiчi, зауваження про зустрiч — чи вiдбулась та iнше). 
--Визначне функцiї для : пошуку iнформацiї за номером телефону;

type People = String
type Phone = String 
type Info = String 
type Place = String 
type DayMonthYear = (Int, Int, Int)

data Notebook = Remind People DayMonthYear 
            |Contacts Phone People
            |Meeting DayMonthYear Place Info
            deriving Show 

getByPhone:: [Notebook]->Phone->Maybe People
getByPhone l p = getB l
    where   getB [] = Nothing
            getB (Contacts p' x:xs)
                |p'==p=Just x
                |otherwise = getB xs
            getB(_:xs) = getB xs

testData :: [Notebook]
testData = [
    Remind "Artemiuk I.O" (7,10,2021), 
    Remind "Prihodko V.V." (4, 8,2021),
    Contacts "099-200-23-16" "shaparenko A.A.",
    Contacts "095-952-88-33" "Ryabin V.I.",
    Contacts "095-388-54-03" "Strykina A.D.",
    Meeting  (4, 8,2021) "Kyiv" "Zystrich Vidbylasya"
        ]

--Tests:
-- *Main> getByPhone testData "099-200-23-16"
-- Just "shaparenko A.A."
-- *Main> getByPhone testData "095-952-88-33"             
-- Just "Ryabin V.I."
-- *Main> getByPhone testData "095-388-54-03"
-- Just "Strykina A.D."

 --Висновок: Під час лабораторної роботи я мала змогу познайомитися та 
 --імплементувати класи типів мови Haskell. Також ознайомилася з системою
 --типів та класів типів, визначила власні функції для нового типу. 