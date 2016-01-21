module SudokuSorted where

import Data.List

numlist = [1..9]

block pos@(i,j) = [(x+dx,y+dy) |dx<-[0..2],dy<-[0..2]]
    where x = i - mod i 3
          y = j - mod j 3

line pos@(i,_) = [(i,dy)| dy <- [0..9]]

col pos@(_,j) = [(dx,j)| dx<-[0..9]]

allp pos = concat [block pos,line pos,col pos]

get pos all = (map head).group.sort $ [ k | (xy,k)<- all, elem xy $ allp pos ]

refresh pos k all = [ if (not $ elem xy $ allp pos) || elem k ks then (xy,ks) else (xy,k:ks) | (xy,ks) <- all]

solve ([],taken) = [taken]
solve (empty,taken) = concatMap solve [ (refresh pos i rst,(pos,i):taken) | i<-avail]
    where (pos,used):rst = sortBy (\a b -> compare (length.snd $ b) (length.snd $ a)) empty
          avail = [ i |i<-numlist, not $ elem i used]
          
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
