module Nqueen (
    nQueen
) where

nQueen :: Int -> [[Int]]
nQueen n=nQueenStep optList n emptyMaskList
            where optList=[1,2..n]

data Mask=Up Int|Down Int|Parallel Int
type MaskList=[Mask]
emptyMaskList=[]

goForward (Up x)=(Up (x+1))
goForward (Down x)=(Up (x-1))
goForward x=x

nQueenStep :: [Int]->Int->MaskList->[[Int]]

nQueenStep opt n ml=do 
    lt'<-mask ml opt
    x<-lt'
    if n==1 then [[x]] 
            else 
                do 
                    solve<-nQueenStep opt (n-1) (map goForward ((Parallel x):(Up x):(Down x):ml))
                    [x:solve]
