!initialize
Moment0_IN = 0._KIND_R

DO m=1,SIZE(x,2)

 coeff = 0._KIND_R
 DO d=1,SIZE(u)
  coeff = coeff + x(d,m)*u(d)
 END DO
 
 !outgoing only
 IF( coeff<=0._KIND_R )THEN
  Moment0_IN = Moment0_IN + f(m)*w(m)
 END IF
END DO
