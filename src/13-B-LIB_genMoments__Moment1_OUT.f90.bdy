!initialize
Moment1_OUT = 0._KIND_R

DO m=1,SIZE(x,2)

 coeff = 0._KIND_R
 DO d=1,SIZE(u)
  coeff = coeff + x(d,m)*u(d)
 END DO
  
 !outgoing only
 IF( coeff>0._KIND_R )THEN
  store = f(m)*w(m)
  DO d=1,SIZE(x,1)
   Moment1_OUT(d) = Moment1_OUT(d) + x(d,m)*store
  END DO
 END IF
END DO
