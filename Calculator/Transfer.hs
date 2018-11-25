module Transfer (
    transfer )where

import DataType
transfer :: String ->Exp
transfer x=y
    where (y,_)=(transferf [] . transferm . words) x

transferm :: [String]->[Exp]
transferm ("(":x)=Begin:(transferm x)
transferm (")":x)=End:(transferm x)
transferm []=[]
transferm (x:y)= let new=reads x ::[(Double,String)] 
                     get ((x,_):_)=x  in
    if new==[] then (Opt x):(transferm y)
    else (Num (get new)):(transferm y)

transferf :: [Exp]->[Exp]->(Exp,[Exp])
transferf fromer []=((Exp fromer),[])
transferf former (End:rest)=((Exp former),rest)
transferf fromer (Begin:x)=transferf (fromer++[exp]) rest
                        where (exp,rest)=transferf [] x
transferf former (x:rest)=transferf (former++[x]) rest