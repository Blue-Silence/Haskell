module Tools(
    car
   ,cdr
   ,belong
   ,getopt
)where

import DataType

car (x:y)=x
cdr (x:y)=y

belong (OptList _ x)=belongs x
getopt (OptList _ x) =get_opt x 

belongs :: [([Char],(Double->Double->Double))]->String->Bool
belongs [] _=False
belongs ((name,_):rest) opt=if name==opt then True
                           else belongs rest opt

get_opt ((name,proc):rest) opt=if name==opt then proc
                               else get_opt rest opt

add :: OptList_All->(Int,([Char],(Double->Double->Double)))->OptList_All
add z@(x@(OptList lev list):rest) y@(level,item)
    |lev==level=((OptList lev item:list):rest)
    |lev>level=x:(add rest y)
    |lev<level=(OptList level [item]):z
add [] (level,item)= [(OptList level [item])]


