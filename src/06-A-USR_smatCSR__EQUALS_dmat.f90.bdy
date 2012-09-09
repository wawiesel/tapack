nzmax = COUNT(A/=ZERO)

CALL DEALLOCATE( S )

IF( SIZE(A,1)/=SIZE(A,2) )THEN
 !non-square matrix
 RETURN
END IF

CALL ALLOCATE( S, SIZE(A,1) , nzmax )

nrow = UBOUND(A,1)
ncol = UBOUND(A,2)

S % IA(1  ) = 1
S % N = nrow
k = 0

DO i = 1, nrow
   DO j = 1 , ncol 
      IF( A(i,j)/=ZERO )THEN
       k           = k + 1   
       S % JA(k  ) = j
       S % A (k)   = A(i,j)
  END IF 
 END DO
 S % IA(i+1) = k + 1
END DO
