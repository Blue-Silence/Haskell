module Tools(
    car
   ,cdr
   ,belong
   ,getopt
)where

car (x,y)=x
cdr (x,y)=y

belong (OptList _ x)=belongs x
getopt (OptList _ x) =get-opt x 

belongs [] _=False
belongs ((name,_):rest) opt=if name==opt then True
                           else belong rest opt

get-opt ((name,proc):rest) opt=if name==opt then proc
                               else get-opt rest opt

