import SudokuGen
import SudokuSorted
import SudokuHelper

main = do
    puzzle <- sudokuGen
    putStrLn (pretty $ puzzle)
    putStrLn (pretty $ solveStr puzzle)
