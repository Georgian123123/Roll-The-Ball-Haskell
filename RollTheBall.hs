{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

arrs :: (A.Array (Int, Int) Cell)
arrs = A.array ((0, 0), (1, 1)) 
              [((0, 0), (Cell (0, 0) startRight)), ((0, 1), (Cell (0, 1) topRight)),
               ((1, 0), (Cell (1, 0) winRight)), ((1, 1), (Cell (1, 1) botRight))]


data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {position :: Position, dtype :: Char}--TODO
    deriving(Eq, Ord)
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {n :: Position , cells ::  A.Array (Int, Int) Cell}--TODO
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level
    where 
        show (Level nr allCells) = foldr (\x acc -> if ((snd (position x) == (snd nr) - 1) && (snd (position x)) == 0)
                                                    then "\n" ++ [(dtype) x] ++ acc
                                                    else if ((snd (position x)) == 0)
                                                        then "\n" ++ [(dtype) x] ++ acc
                                                        else [(dtype) x] ++ acc) "\n" 
                                                                            (A.elems allCells)
     

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}
makeArray ::  (Int, Int) -> A.Array(Int, Int) Cell
makeArray x = array ((0, 0), x)[((i, j), (Cell (i, j) emptySpace)) | i <- [0..(fst x)], j <- [0..(snd x)]]

emptyLevel :: Position -> Level
emptyLevel pos = Level pos (makeArray pos)

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}
elemAt :: (A.Array (Int, Int) Cell) -> (Int, Int) -> Cell
elemAt arr pos = arr A.! pos

updateArr :: (A.Array (Int, Int) Cell) -> Position -> Char -> (A.Array (Int, Int) Cell)
updateArr arr pos1 tp = arr A.// [(pos1, (Cell pos1 tp))]

updateArr2 :: (A.Array (Int, Int) Cell) -> Position -> Position -> Char -> (A.Array (Int, Int) Cell)
updateArr2 arr pos1 pos2 tp = arr A.// [(pos1, (Cell pos1 tp)), (pos2, (Cell pos2 emptySpace))]



addCell :: (Char, Position) -> Level -> Level
addCell wh (Level nr allCells) = if ((fst (snd wh)) >= 0 && (fst (snd wh)) <= fst nr && (snd (snd wh)) >= 0 && (snd (snd wh)) <= snd nr)
                                   then if ((dtype) (elemAt allCells (snd wh)) == emptySpace)
                                        then  (Level nr (updateArr allCells (snd wh) (fst wh)))
                                        else (Level nr allCells)
                                    else (Level nr allCells)


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel p list = (foldl (\acc x -> addCell x acc) (emptyLevel p) list)


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir (Level nr allCells) = if ((fst pos) >= 0 && (fst pos) <= fst nr
                                            && snd pos >= 0 && snd pos <= snd nr)
                                        then if ((dtype) (elemAt allCells pos) == winUp ||
                                                (dtype) (elemAt allCells pos) == winDown ||
                                                (dtype) (elemAt allCells pos) == winLeft ||
                                                (dtype) (elemAt allCells pos) == winRight ||
                                                (dtype) (elemAt allCells pos) == startUp ||
                                                (dtype) (elemAt allCells pos) == startDown ||
                                                (dtype) (elemAt allCells pos) == startLeft ||
                                                (dtype) (elemAt allCells pos) == startRight)
                                                then (Level nr allCells)
                                                 else if (dir == North && (fst pos) - 1 >= 0 && 
                                                        (dtype) (elemAt allCells (fst pos - 1, snd pos)) == emptySpace)
                                                        then (Level nr (updateArr2 allCells (((fst pos) - 1), (snd pos)) pos
                                                                    ((dtype) (elemAt allCells pos))))
                                                                    
                                                        else if (dir == South && (fst pos) + 1 <= fst nr && 
                                                                (dtype) (elemAt allCells (fst pos + 1, snd pos)) == emptySpace)
                                                                then (Level nr (updateArr2 allCells (((fst pos) + 1), (snd pos)) pos
                                                                    ((dtype) (elemAt allCells pos))))

                                                                 else if (dir == East && (snd pos) + 1 <= snd nr && 
                                                                        (dtype) (elemAt allCells (fst pos, (snd pos) + 1)) == emptySpace)
                                                                         then (Level nr (updateArr2 allCells ((fst pos), ((snd pos) + 1)) pos
                                                                             ((dtype) (elemAt allCells pos))))

                                                                         else if (dir == West && (snd pos) - 1 >= 0 && 
                                                                             (dtype) (elemAt allCells (fst pos, (snd pos) - 1)) == emptySpace)
                                                                             then (Level nr (updateArr2 allCells ((fst pos), ((snd pos) - 1)) pos
                                                                             ((dtype) (elemAt allCells pos))))
                                                                            else (Level nr allCells)
                                                else (Level nr allCells)


{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 dir =  let x = (dtype) cell2
                                in if ((dtype)cell1 == horPipe)
                                    then if (dir == West && (x == horPipe || x == botLeft || x == topLeft || x == winRight))
                                        then True
                                        else if (dir == East && (x == horPipe || x == botRight || x == topRight) || x == winLeft)
                                            then True
                                            else False
                                else if ((dtype)cell1 == verPipe)
                                     then if (dir == North && (x == verPipe || x == topLeft || x == topRight || x == winDown))
                                        then True
                                        else if (dir == South && (x == verPipe || x == botRight || x == botLeft || x == winUp))
                                            then True
                                            else False
                                else if ((dtype)cell1 == topLeft)
                                     then if (dir == South && (x == verPipe || x == botRight || x == botLeft || x == winUp))
                                        then True
                                        else if (dir == East && (x == horPipe || x == botRight || x == topRight || x == winLeft))
                                            then True
                                            else False
                                else if ((dtype)cell1 == botLeft)
                                    then if (dir == North && (x == verPipe || x == topLeft || x == topRight || x == winDown))
                                        then True
                                        else if (dir == East && (x == horPipe || x == botRight || x == topRight || x == winLeft))
                                            then True
                                            else False
                                else if ((dtype)cell1 == botRight)
                                    then if (dir == North && (x == verPipe || x == topLeft || x == topRight || x == winDown))
                                        then True
                                        else if (dir == West && (x == horPipe || x == botLeft || x == topLeft || x == winRight))
                                            then True
                                            else False
                                else if ((dtype)cell1 == topRight)
                                    then if (dir == South && (x == verPipe || x == botLeft || x == botRight || x == winUp))
                                        then True
                                        else if (dir == West && ((x == horPipe || x == botLeft || x == topLeft || x == winRight)))
                                            then True
                                            else False
                                else if ((dtype)cell1 == startUp)
                                    then if (dir == North && ((x == verPipe) || (x == topLeft) || x == topRight))
                                        then True
                                        else False
                                else if ((dtype)cell1 == startDown)
                                    then if (dir == South && (x == verPipe || x == botRight || x == botLeft))
                                        then True
                                        else False
                                else if ((dtype)cell1 == startLeft)
                                    then if (dir == West && (x == horPipe || x == botLeft || x == topLeft))
                                        then True
                                        else False
                                else if ((dtype)cell1 == startRight)
                                    then if (dir == East && (x == horPipe || x == botRight || x == topRight))
                                        then True
                                        else False
                                else False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

findStart :: [Cell] -> Cell
findStart arr
        | arr == []   =   (Cell (0, 0) emptySpace)
        | ((dtype)(head arr) `elem` startCells) = head arr
        | otherwise = findStart (tail arr)

wonAux :: A.Array(Int, Int) Cell -> Cell -> Directions -> (Int, Int) -> Bool
wonAux arr cell dir nr
        | ((dtype) cell) `elem` winningCells = True
        | (fst ((position) cell) - 1) >= 0 &&  (dir /= North) && ((dtype)cell `elem` [verPipe, botLeft, botRight]) && (connection cell (elemAt arr (((fst ((position) cell)) - 1),(snd ((position) cell)))) North == True)
                  = (wonAux arr (elemAt arr ((fst ((position) cell) - 1), (snd ((position) cell)))) South nr)
        | (fst ((position) cell) + 1) <= (fst nr) &&  (dir /= South) && ((dtype)cell `elem` [verPipe, topLeft, topRight]) && (connection cell (elemAt arr (((fst ((position) cell)) + 1),(snd ((position) cell)))) South == True)
                  = (wonAux arr (elemAt arr ((fst ((position) cell) + 1), (snd ((position) cell)))) North nr)
        | (snd ((position) cell) + 1) <= (snd nr) &&  (dir /= East) && ((dtype)cell `elem` [horPipe, topLeft, botLeft]) && (connection cell (elemAt arr (((fst ((position) cell))),(snd ((position) cell) + 1))) East == True)
                  = (wonAux arr (elemAt arr ((fst ((position) cell)), (snd ((position) cell)) + 1)) West nr)
        | (snd ((position) cell) - 1) >= 0 &&  (dir /= West) && ((dtype)cell `elem` [horPipe, topRight, botRight]) && (connection cell (elemAt arr (((fst ((position) cell))),(snd ((position) cell) - 1))) West == True)
                  = (wonAux arr (elemAt arr ((fst ((position) cell)), (snd ((position) cell)) - 1)) East nr)
        | otherwise = False

wonLevel :: Level -> Bool
wonLevel (Level nr arr) 
            | ((dtype) cell) == startUp && (connection cell (elemAt arr (((fst ((position) cell)) - 1),(snd ((position) cell)))) North == True)
                      = (wonAux arr (elemAt arr ((fst ((position) cell) - 1), (snd ((position) cell)))) South nr)
            | ((dtype) cell) == startDown && (connection cell (elemAt arr (((fst ((position) cell)) + 1),(snd ((position) cell)))) South == True)
                      = (wonAux arr (elemAt arr ((fst ((position) cell) + 1), (snd ((position) cell)))) North nr)  
            | ((dtype) cell) == startLeft &&  (connection cell (elemAt arr (((fst ((position) cell))),(snd ((position) cell) - 1))) West == True)
                      = (wonAux arr (elemAt arr ((fst ((position) cell)), (snd ((position) cell)) - 1)) East nr)
            | ((dtype) cell) == startRight &&  (connection cell (elemAt arr (((fst ((position) cell))),(snd ((position) cell) + 1))) East == True)
                      = (wonAux arr (elemAt arr ((fst ((position) cell)), (snd ((position) cell)) + 1)) West nr)
            | otherwise = False
                      where cell = (findStart (A.elems arr))

instance ProblemState Level (Position, Directions) where
    successors (Level nr allCells) = filter (\x-> (Level nr allCells) /= (snd x))
                                                    (map (\x -> ((((position) x), North), (moveCell ((position) x) North (Level nr allCells)))) (A.elems allCells) ++
                                                       map (\x -> ((((position) x), South), (moveCell ((position) x) South (Level nr allCells)))) (A.elems allCells) ++
                                                          map (\x -> ((((position) x), West), (moveCell ((position) x) West (Level nr allCells)))) (A.elems allCells) ++
                                                            map (\x -> ((((position) x), East), (moveCell ((position) x) East (Level nr allCells)))) (A.elems allCells))
    isGoal x = wonLevel x 
    reverseAction x 
                | (snd (fst x)) == South  =  ((((fst (fst (fst x))) + 1, snd (fst (fst x))), North), snd x)
                | (snd (fst x)) == North  =  ((((fst (fst (fst x))) - 1, snd (fst (fst x))), South), snd x)
                | (snd (fst x)) == East  =  ((((fst (fst (fst x))), snd (fst (fst x)) + 1), West), snd x)
                | (snd (fst x)) == West  =  ((((fst (fst (fst x))), snd (fst (fst x)) - 1), East), snd x)
                | otherwise = x