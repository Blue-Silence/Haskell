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




