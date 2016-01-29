module SudokuAmb where

import Data.List
import Control.Monad

numlist = [1..9]

allp (i,j) = concat [block,line,col]
    where block = [(i + dx - mod i 3,j + dy - mod j 3) |dx<-[0..2],dy<-[0..2]]
          line = [(i,dy)| dy <- [0..9]]
          col = [(dx,j)| dx<-[0..9]]

get pos all = (map head).group.sort $ [ k | (xy,k)<- all, elem xy $ allp pos ]

refresh pos k all = [ if (not $ elem xy $ allp pos) || elem k ks then (xy,ks) else (xy,k:ks) | (xy,ks) <- all]

solve ([],taken) = [taken]
solve (empty,taken) = concat $ do 
    i<-numlist
    guard $ (not $ elem i used)
    return $ solve (refresh pos i rst,(pos,i):taken)
    where (pos,used):rst = sortBy (\a b -> compare (length.snd $ b) (length.snd $ a)) empty

solveStr :: [Int] -> [Int]
solveStr = (getout 0). head . solve . fill . (getin 0)

fill (empty,taken) = ([(pos,get pos taken) | pos <- empty],taken)

getin i [] = ([],[])
getin i (x:xs) = if x/=0 then (empty,(pos,x):taken) 
        else (pos:empty,taken)
    where (empty,taken) = getin (i+1) xs
          pos = (div i 9, mod i 9)

getout 81 taken = []
getout i taken = k:(getout (i+1) taken)
    where pos = (div i 9, mod i 9)
          k = head [ k|(xy,k)<-taken,xy==pos]
