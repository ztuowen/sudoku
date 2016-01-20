import SudokuGen
import SudokuSorted

main = do
    puzzle <- sudokuGen
    putStrLn (show puzzle)
    putStrLn (show $ solveStr puzzle)
