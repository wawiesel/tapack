!initialize
Moment1_NormalIN = 0._KIND_R

DO m=1,SIZE(x,2)

 coeff = 0._KIND_R
 DO d=1,SIZE(u)
  coeff = coeff + x(d,m)*u(d)
 END DO
 
 !incoming only
 IF( coeff<=0._KIND_R )THEN
  Moment1_NormalIN = Moment1_NormalIN + coeff*f(m)*w(m)
 END IF
END DO
