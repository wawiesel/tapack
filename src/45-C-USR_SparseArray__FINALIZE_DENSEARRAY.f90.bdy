!get sizes
IF( .NOT.ASSOCIATED(DenseArray) )THEN
 N(1) = 0
 N(2) = 0
ELSE
 N(1) = SIZE(DenseArray,1)
 N(2) = SIZE(DenseArray,2)
END IF

!get counter
N2 = N(2)

!first handle the second dimension
DO i=N(2),1,-1
 IF( ALL( DenseArray(:,i)==ERROR_ ) )THEN
  N2 = N2-1
 ELSE
  EXIT
 END IF
END DO
CALL REALLOCATE( DenseArray , (/0,N2-N(2)/) )

IF( .NOT.ASSOCIATED(DenseArray) )THEN
 N(1) = 0
 N(2) = 0
ELSE
 N(1) = SIZE(DenseArray,1)
 N(2) = SIZE(DenseArray,2)
END IF

!now handle the first dimension
N1 = 0
DO i=1,N(2)

 N1 = MAX( N1 , SIZEa(DenseArray(:,i)) )
END DO
CALL REALLOCATE( DenseArray , (/N1-N(1),0/) )

