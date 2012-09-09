!initialize
Moment2_Dim = 0._KIND_R

DO m=1,SIZE(x,2)
 coeff = f(m)*w(m)
 Moment2_Dim = Moment2_Dim + x(Dim1,m)*x(Dim2,m)*coeff
END DO
