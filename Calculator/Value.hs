module Value(
    value
)where

import DataType
import Transfer
import Tools

valuef :: OptList_All->OptList_All->Exp->Exp
valuef all _ (Num value)=(Num value)
valuef all _ (Exp x:[])=valuef all all x
valuef all listnow (Exp (x:opt:y:z))=if belong (car listnow) opt then value all listnow (Exp (Num ((getopt listnow opt) valuex valuey)):z)
                                     else value all (cdr listnow) (Exp x:opt:(value all listnow (y:z)))                      
            where (Num valuex)=valuef all all x
                  (Num valuey)=valuef all all y




