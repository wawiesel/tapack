!initialize
Moment2 = 0._KIND_R


DO m=1,SIZE(x,2)
 
 store = f(m)*w(m)

 !! diagonal of tensor
 DO d=1,SIZE(x,1)
  Moment2(d) = Moment2(d) + x(d,m)*x(d,m)*store
 END DO

 !! remainder (symmetric off-diagonals)
 d = SIZE(x,1)
 DO d1=1,SIZE(x,1)
  DO d2=1,SIZE(x,1)
   IF( d1<=d2 )CYCLE
   d = d + 1
   Moment2(d) = Moment2(d) + x(d1,m)*x(d2,m)*store
  END DO
 END DO

END DO
