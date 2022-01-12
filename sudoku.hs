type Value = Int  -- valorile din matricea Sudoku (0..9), unde 0- casuta libera
type Grid = [[Value]] -- gridul(matricea) Sudoku reprezentata precum o lista de liste de valori
type RowCol = (Int, Int) -- locatia in grid 

inputGrid :: [[Int]]  -- grid de test
inputGrid = [[3, 0, 0,  0, 0, 0,  8, 0, 0],
             [5, 0, 0,  0, 2, 8,  1, 0, 7],
             [4, 0, 8,  0, 0, 0,  0, 6, 9],

             [8, 1, 0,  9, 0, 7,  6, 0, 0],
             [9, 6, 0,  0, 5, 0,  0, 8, 1],
             [0, 0, 7,  1, 0, 6,  0, 9, 3],

             [6, 3, 0,  0, 0, 0,  2, 0, 8],
             [7, 0, 5,  3, 4, 0,  0, 0, 6],
             [0, 0, 2,  0, 0, 0,  0, 0, 5]]

-- returneaza valoarea de la o pozitie specifica din grid
getValueAt :: Grid -> RowCol -> Value
getValueAt grid (row, column) = grid !!  row !! column

--returneaza valorile dintr-o linie specificata
getValuesAtRow :: Grid -> Int -> [Value]
getValuesAtRow grid row = grid !! row

-- returneaza valorile dintr-o coloana specificata
getValuesAtColumn :: Grid -> Int -> [Value]
getValuesAtColumn grid column = [getValueAt grid (row, column) | row <- [0..8]]

--returneaza locatiile unde exista spatiu gol (valoarea 0) in grid
getEmptyValues :: Grid -> [RowCol]
getEmptyValues grid = [(row, col) | row <- [0..8],
                                    col <- [0..8],
                                    getValueAt grid (row, col) == 0]

--returneaza cele 9 valori dintr-o casuta care contine randul si coloana specificata 
getValuesFromSquare :: Grid -> RowCol -> [Value]
getValuesFromSquare  grid (row, column) =
    let minimumRow = (row `div` 3) * 3
        minimumColumn =(column `div` 3) * 3
        maximumRow = minimumRow + 2
        maximumColumn = minimumColumn + 2
    in [getValueAt grid (row1, column1) | row1 <-[minimumRow..maximumRow],
                                          column1 <- [minimumColumn..maximumColumn]]

--returneaza true in cazul in care o valoare poate fi inserata la o anumita pozitie, false in mod contrar
valueCanBePlaced :: Grid -> RowCol -> Value -> Bool 
valueCanBePlaced grid (row, column) val =
    valueNotFoundInRow row val grid && valueNotFoundInCol column val grid && valueNotFoundInSquare (row, column) val grid
    where valueNotFoundInRow row value grid = notElem val $ getValuesAtRow grid row 
          valueNotFoundInCol column value grid = notElem val $ getValuesAtColumn grid column 
          valueNotFoundInSquare (row, column) value grid = notElem val $ getValuesFromSquare grid (row, column) 

--returneaza gridul cu noua valoare adaugata 
putValueInGrid :: Grid -> RowCol -> Value -> Grid
putValueInGrid grid (row, column) val = putValue newRow row grid
    where putValue row2 row1 grid1 = [if i == row1 then row2 else grid1 !! i | i<-[0..(length grid1 -1)]]
          newRow = putValue val column (grid !! row)

--returneaza o solutie 
        --selectare locatie goala-> cauta care valori pot fi introduse -> apelare recursiva 
getSolutions:: Grid -> [RowCol] -> [Grid]
getSolutions grid [] = [grid]
getSolutions grid (x:xs) = concatMap (\grid -> getSolutions grid xs) grids
    where  values = [val | val <-[1..9],
                           valueCanBePlaced grid x val]
           grids = map (\val -> putValueInGrid grid x val) values

--returneaza toate solutiile 
getAllSolutions :: Grid -> [Grid]
getAllSolutions grid = getSolutions grid $ getEmptyValues grid 

--returneaza prima solutie dintr-o lista sau nimic 
getFirstOrNothing :: [a] -> Maybe a
getFirstOrNothing [] = Nothing
getFirstOrNothing (x:xs) = Just x

--returneaza prima solutie sau nimic daca nu a fost gasita nici o solutie 
getFirstSolutionOrNothing :: Grid -> Maybe Grid
getFirstSolutionOrNothing grid = getFirstOrNothing $ getAllSolutions grid

--afisare grid
printGrid:: Maybe Grid -> IO()
printGrid (Just rows) = mapM_ (\x -> putStrLn $ show x) rows
printGrid Nothing = putStrLn "Solution Not Found!"

mainFunction = printGrid grid
    where grid = getFirstSolutionOrNothing inputGrid