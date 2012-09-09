!initialize
Moment1_Normal = 0._KIND_R

DO m=1,SIZE(x,2)
 coeff = 0._KIND_R
 DO d=1,SIZE(u)
  coeff = coeff + x(d,m)*u(d)
 END DO
 Moment1_Normal = Moment1_Normal + coeff*f(m)*w(m)
END DO
