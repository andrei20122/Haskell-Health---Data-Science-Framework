
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Distribution.Simple.Program.HcPkg (list)


type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
toInt :: [String ] -> [Int]
toInt = map read
-- functia pentru a transforma lista de stringuri in int

medie_arm :: [Int ] -> Float
medie_arm [] = 0.0
medie_arm lista = fromIntegral  (sum lista) / fromIntegral (length lista)
--media aritmetica care imi aduna int urile si le imparte la nr lor 


compute_average_steps :: Table -> Table
compute_average_steps m = [ "Name", "Average Number of Steps"] : map(\x -> [head x, printf "%.2f" (medie_arm (map read (tail x)))]) (tail  m)


-- Task 2

-- Number of people who have achieved their goal:

op :: Int   -> Int -> Int
op ele acc =  ele + acc

get_passed_people_num :: Table -> Int
get_passed_people_num m = length (filter( \x ->   foldr op 0 (toInt(tail x)) >= 1000  ) (tail m))


-- Percentage of people who have achieved their:


medie :: Table -> Float
medie m = fromIntegral   (get_passed_people_num m) / fromIntegral (length  m)

percent :: Float  -> Float
percent x = fromIntegral (round (x*100))/100.0

get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = percent (medie m)

-- Average number of daily steps
sumAcc :: [String] -> Int 
sumAcc [] = 0
sumAcc z = foldr(\y acc2 -> acc2 + read y) 0  (tail z)

get_steps_avg :: Table -> Float
get_steps_avg m = percent (fromIntegral (  foldr (\x acc -> acc +(sumAcc x)) 0 (tail m)) / fromIntegral (length (tail m)))


-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m =  ["H10","H11","H12","H13","H14","H15","H16","H17"]:[map(printf "%.2f" . medie_arm . toInt) ( tail(transpose (tail m)))]


-- Task 4

vam :: Table -> Row
vam [] = []
vam t =   "VeryActiveMinutes": show (length (filter (\x-> 0 <= read x && read x < 50) (head (drop 3 (transpose (tail t)))) )  )
  : show (length (filter (\x-> 50 <= read x && read x < 100) (head (drop 3 (transpose (tail t)))) )  )
  : show (length (filter (\x-> 100 <= read x && read x < 500) (head (drop 3 (transpose (tail t)))) )  ):[]

-- functia pentru VeryActiveMinutes ^^

fam :: Table -> Row
fam [] = []
fam t = "FairlyActiveMinutes": show (length (filter (\x-> 0 <= read x && read x < 50) (head (drop 4 (transpose (tail t)))) )  )
  : show (length (filter (\x-> 50 <= read x && read x < 100) (head (drop 4 (transpose (tail t)))) )  )
  : show (length (filter (\x-> 100 <= read x && read x < 500) (head (drop 4 (transpose (tail t)))) )  ):[]

-- functia pentru FairlyActiveMinutes ^^

lam :: Table -> Row
lam [] = []
lam t = "LightlyActiveMinutes": show (length (filter (\x-> 0 <= read x && read x < 50) (head (drop 5 (transpose (tail t)))) )  )
  : show (length (filter (\x-> 50 <= read x && read x < 100) (head (drop 5 (transpose (tail t)))) )  )
  : show (length (filter (\x-> 100 <= read x && read x < 500) (head (drop 5 (transpose (tail t)))) )  ) : []

-- functia pentru LightlyActiveMinutes ^^

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"]:[ vam m, fam m, lam m]


-- Task 5

sortare :: Row -> Row -> Ordering
sortare [] [] = EQ
sortare r1 r2=
  if (read (head ( (tail   r1)))::Int) == (read (head (tail  r2))::Int) then strcmp(head  r1) (head  r2)
  else if (read (head ( tail   r1))::Int) > (read (head(tail  r2))::Int) then GT
  else  LT

strcmp :: String -> String -> Ordering
strcmp [] [] = EQ
strcmp a [] = GT
strcmp [] a = LT
strcmp s1 s2 =
  if head s1 == head s2 then strcmp  (tail s1) (tail s2)
  else if head s1 > head s2 then GT
  else  LT

get_ranking :: Table -> Table
get_ranking m =  ["Name","Total Steps"] : (map(take 2 )(sortBy sortare (tail m)))


-- Task 6

sortareDifference :: Row -> Row -> Ordering
sortareDifference [] [] = EQ
sortareDifference r1 r2 =
  if (read (head ( (drop 3  r1)))::Float) == (read (head (drop 3 r2))::Float) then strcmp(head r1) (head r2)
  else if (read (head ( drop 3  r1))::Float) > (read (head(drop 3 r2))::Float) then GT
  else  LT




avgfirst4 :: Row -> Float
avgfirst4 [] = 0.00
avgfirst4 a =  medie_arm(toInt (take 4 (tail a)))

avglast4 :: Row -> Float
avglast4 [] = 0.00
avglast4 b = medie_arm(toInt (drop 4 (tail b)))

differenceR :: Row -> Float
differenceR [] = 0.00
differenceR r = abs (avgfirst4 r - avglast4 r)

get_steps_diff_table :: Table -> Table
get_steps_diff_table m =  ["Name","Average first 4h","Average last 4h","Difference"] :
  sortBy sortareDifference (map(\x -> head x : ( printf "%.2f" ( avgfirst4 x)) :
   (printf "%.2f" ( avglast4 x)): (printf "%.2f" ( differenceR  x)  ) :[] )(tail m))


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map(map f ) ( m)


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map f (tail m)


toFloat :: Int -> Float 
toFloat 0 = 0.00
toFloat i = fromIntegral i

get_sleep_total :: Row -> Row
get_sleep_total r = head r  :  (printf "%.2f"  (toFloat(foldl (+) 0 ( toInt (tail r))))) : []


{-
    TASK SET 2
-}

-- Task 1

strcmp2 :: String -> String -> Ordering
strcmp2 [] [] = EQ
strcmp2 a [] = GT
strcmp2 [] a = LT
strcmp2 s1 s2 =
  if head s1 == head s2 then strcmp2  (tail s1) (tail s2)
  else if head s1 > head s2 then GT
  else  LT
  -- functie luata din tema trecuta 

sort_nr :: String -> String -> Ordering
sort_nr [] [] = EQ
sort_nr a [] = GT
sort_nr [] a = LT
sort_nr s1 s2 = if (read  s1::Float ) == (read  s2::Float )  then EQ
                else if (read s1::Float ) > (read s2::Float )  then GT
                else LT
--functia ce imi compara numerele date ca stringuri ^^ 

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False
--functia care verifica daca  un string e numar

findPs :: ColumnName -> [String ] -> Int
findPs _ [] = 0
findPs cn (x:xs) = if x == cn then 0
                  else 1 + findPs cn xs
--functia care imi returneaza poz lui ColumnName intr o lista 


sortareColoane :: Int -> Row -> Row -> Ordering
sortareColoane _ [] [] = EQ
sortareColoane  nr r1 r2 = if isNumber ( r1 !! nr) then -- daca elem de pe poz nr din r1 este numar 
                                if  sort_nr (r1 !! nr) (r2 !! nr) == EQ then -- daca elem de la poz nr din r1 si r2 sunt egale 
                                    if isNumber ( r1 !! 0) then sort_nr (r1 !! 0) (r2 !! 0) -- daca elem din prima coloana este nr compari 
                                    else strcmp2 (r1 !! 0) (r2 !! 0) -- daca nu compari ca 2 stringuri
                                else  sort_nr (r1 !! nr) (r2 !! nr)-- daca nu sortezi normal ca numere 
                          else if strcmp2 (r1 !! nr) (r2 !! nr) == EQ then -- aici daca 2 stringuri sunt egale fac ce am facut mai sus
                                if isNumber ( r1 !! 0) then sort_nr (r1 !! 0) (r2 !! 0)
                                else strcmp2 (r1 !! 0) (r2 !! 0)
                          else strcmp2 (r1 !! nr) (r2 !! nr)
  

tsort :: ColumnName -> Table -> Table
tsort column table =   head table : sortBy ( sortareColoane (findPs column (head table))) (tail table) 

-- Task 2

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : merge xs (y:ys)
-- imi concateneaza 2 liste

vunion :: Table -> Table -> Table
vunion t1 t2 = if head  t1 == head  t2 then merge  t1 (tail t2)
              else t1
              -- else error "baaa :(((("

-- Task 3



umplereRanduri :: Table-> Int -> Table
umplereRanduri t 0 = t
umplereRanduri t nrRowAdd = umplereRanduri t (nrRowAdd- 1) ++ [nrStringGol (length (transpose t))]  
  where nrStringGol :: Int -> Row
        nrStringGol 0 = []
        nrStringGol s = [""] ++ nrStringGol (s-1)
--fc care imi ia un tabel si cate randuri vreau sa pun in el si return another table
--nrStringGol imi ia un nr si vreau sa mi returneze un rand cu atatea spatii goale 

hunion :: Table -> Table -> Table
hunion t1 t2 = if length t1 > length t2 then transpose ( (transpose t1) ++ (transpose (umplereRanduri t2 diferenta )))
              else transpose ( (transpose (umplereRanduri t1 diferenta )) ++ (transpose t2) )
    where diferenta = abs (length t1 - length t2 )
    -- daca t1 este mai mare decat t2 la lungime atunci eu practic concatenez cele 2 tabele transpuse si dupa fac transpusa lor si o sa vina exact 
    -- coloanele pe care le vreau ,iar apoi la final adaug cu ajutorul functiei mele (umplereRanduri ) adaug randurile goale de care mai am nevoie
    -- , iar daca nu fac fix inversa 

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = []

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = []

-- Task 6

extractColumn :: Int -> Table -> Row
extractColumn nr t = map (\x -> (head ( drop nr x)  ))  t
-- fc care imi extrage coloana de pe poz nr 

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t =   transpose (map (\x -> extractColumn (findPs x (head t))  t ) columns_to_extract)

-- Task 7
getElemPoz ::  Int  -> [a] -> a
getElemPoz  p list  = list !! p
-- fc care imi ia elementul de la poz p din lista mea list

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t =   head t : filter (\x -> condition(getElemPoz ((findPs key_column (head t))) x)  ) (tail t)