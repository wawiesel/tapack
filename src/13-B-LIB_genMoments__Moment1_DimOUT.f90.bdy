!initialize
Moment1_DimOUT = 0._KIND_R

DO m=1,SIZE(x,2)

 coeff = 0._KIND_R
 DO d=1,SIZE(u)
  coeff = coeff + x(d,m)*u(d)
 END DO
 
 !outgoing only
 IF( coeff>0._KIND_R )THEN
  store = f(m)*w(m)
  Moment1_DimOUT = Moment1_DimOUT + x(Dim,m)*store
 END IF
END DO
