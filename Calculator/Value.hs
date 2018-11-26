module Value(
    value
)where

import DataType
import Tools



value :: OptList_All->OptList_All->Exp->Exp
value all _ (Num value)=(Num value)
value all _ (Exp (x:[]))=value all all x
value all listnow (Exp (x:opt:y:z))=if belong listb optx 
                                     then value all listnow (Exp ([(Num ((getopt listb optx) valuex valuey))] ++ z))
                                     else value all lista (Exp (x:opt:(value all listnow (Exp ([y]++z))):[]))                      
                                            where (Num valuex)=value all all x
                                                  (Num valuey)=value all all y
                                                  (Opt optx)=opt
                                                  listb=car listnow
                                                  lista=cdr listnow





