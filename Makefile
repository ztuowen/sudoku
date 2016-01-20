all:sudoku sgen

sudoku:sudoku.hs SudokuSorted.hs SExmpl.hs
	ghc -O3 sudoku.hs -rtsopts

sgen:sgen.hs SudokuSorted.hs
	ghc -O3 sgen.hs -rtsopts

clean:
	-rm *.{hi,o}
	-rm sudoku
	-rm sgen
