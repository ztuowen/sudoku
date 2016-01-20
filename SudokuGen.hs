module SudokuGen where

import SudokuSorted
import System.Random

start = do 
    i <- shuffle [1..9]
    return.head.solve.fill.(getin 0) $ i ++ [0|r<-[1..72]]

shuffle [] = return []
shuffle xs = do
    i <- getStdRandom(randomR (0,(length xs)-1))::IO(Int)
    let (h:rst) = drop i xs
    oth <- shuffle $ (take i xs) ++ rst
    return (h:oth)

sudokuGen = do
    taken <- start
    tshuffle <-shuffle taken
    return $ getout 0 $ takeall ([],[]) tshuffle

takeall :: ([(Int,Int)],[((Int,Int),Int)]) -> [((Int,Int),Int)] -> [((Int,Int),Int)]
takeall (emp,taken) [] = [(pos,0) |pos <- emp] ++ taken
takeall (emp,taken) non@((pos,k):rst) = if l then takeall (pos:emp,taken) rst
    else takeall (emp,(pos,k):taken) rst
    where l = null.tail $ (solve.fill) (pos:emp,rst++taken)
