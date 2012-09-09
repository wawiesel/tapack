!calculate value based on vertex N -> 1
a = ( Pg(1,N)*Pg(2,1) - Pg(1,1)*Pg(2,N) )
xyP(1) = ( Pg(1,N) + Pg(1,1) )*a
xyP(2) = ( Pg(2,N) + Pg(2,1) )*a

!calculate values based on vertices i-1 -> i, i=2,N
DO i=2,N
 a = ( Pg(1,i-1)*Pg(2,i) - Pg(1,i)*Pg(2,i-1) )
 xyP(1) = xyP(1) + ( Pg(1,i-1) + Pg(1,i) )*a
 xyP(2) = xyP(2) + ( Pg(2,i-1) + Pg(2,i) )*a
END DO

!determine normalization
a = c_1_by_6/ABS(AREA)

!apply normalization
xyP(1) = xyP(1)*a
xyP(2) = xyP(2)*a


