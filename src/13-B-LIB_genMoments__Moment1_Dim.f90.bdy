!initialize
Moment1_Dim = 0._KIND_R

DO m=1,SIZE(x,2)
 coeff = f(m)*w(m)
 Moment1_Dim = Moment1_Dim + x(Dim,m)*coeff
END DO
