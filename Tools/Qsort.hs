module Qsort(qsort,seqqsort)where

hsort x result []=result
hsort x (xl,yl,zl) (y:ys)
    |y>x=hsort x (y:xl,yl,zl) ys
    |y==x=hsort x (xl,y:yl,zl) ys
    |otherwise=hsort x (xl,yl,y:zl) ys

qsort []=[]
qsort (a:[])=[a]
qsort (x:xs)=(qsort xl)++(x:yl)++(qsort zl)
    where (xl,yl,zl)=hsort x ([],[],[]) xs

seqhsort x result []=result
seqhsort x (xl,yl,zl) (y:ys)
    |y>x=seqhsort x (seq (y:xl) (y:xl,yl,zl)) ys
    |y==x=seqhsort x (seq (y:yl) (xl,y:yl,zl)) ys
    |otherwise=seqhsort x (seq (y:zl) (xl,yl,y:zl)) ys

seqqsort []=[]
seqqsort (a:[])=[a]
seqqsort (x:xs)=(seq xl (seqqsort xl))++(x:yl)++(seq zl (seqqsort zl))
    where (xl,yl,zl)=seqhsort x ([],[],[]) xs