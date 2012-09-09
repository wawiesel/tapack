!initialize
Moment0 = 0._KIND_R

DO m=1,SIZE(x,2)
 Moment0 = Moment0 + f(m)*w(m)
END DO