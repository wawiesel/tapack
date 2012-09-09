!initialize
Moment0_OUT = 0._KIND_R

DO m=1,SIZE(x,2)

 coeff = 0._KIND_R
 DO d=1,SIZE(u)
  coeff = coeff + x(d,m)*u(d)
 END DO
  
 !outgoing only
 IF( coeff>0._KIND_R )THEN
  Moment0_OUT = Moment0_OUT + f(m)*w(m)
 END IF
END DO
