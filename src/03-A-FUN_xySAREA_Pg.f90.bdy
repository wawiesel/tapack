!start with vert N->1
sAREA = Pg(1,N)*Pg(2,1) - Pg(1,1)*Pg(2,N)

!now proceed with i-1->i, i=2,N
DO i=2,N
 sAREA = sAREA + Pg(1,i-1)*Pg(2,i) - Pg(1,i)*Pg(2,i-1)
END DO

!apply 1/2 normalization
sAREA = c_1_by_2*sAREA
