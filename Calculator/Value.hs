module Value(
    value
)where

import DataType
import Transfer
import Tools

value :: OptList_All->Exp->Exp
value x=valuef x x

valuef :: OptList_All->OptList_All->Exp->Exp
valuef all _ (Num value)=(Num value)
valuef all _ (Exp (x:[]))=valuef all all x
valuef all listnow (Exp (x:opt:y:z))=if belong listb optx 
                                     then valuef all listnow (Exp ([(Num ((getopt listb optx) valuex valuey))] ++ z))
                                     else valuef all lista (Exp (x:opt:(valuef all listnow (Exp ([y]++z))):[]))                      
                                            where (Num valuex)=valuef all all x
                                                  (Num valuey)=valuef all all y
                                                  (Opt optx)=opt
                                                  listb=car listnow
                                                  lista=cdr listnow





