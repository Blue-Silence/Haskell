f 1=1
f 2=3
f 3=1
g 1=3
g 2=2
g 3=1

xs=[x|x<-[1,2,3],(f.g) x > (g.f) x]