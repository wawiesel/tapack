!initialize
Moment1 = 0._KIND_R

DO m=1,SIZE(x,2)
 
 store = f(m)*w(m)
 
 DO d=1,SIZE(x,1)
  Moment1(d) = Moment1(d) + x(d,m)*store
 END DO

END DO
