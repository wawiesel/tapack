DO n=LBOUND(A,2),UBOUND(A,2)
 DO m=LBOUND(A,1),UBOUND(A,1) 
  WRITE(Unit,*)A(m,n)
 END DO
END DO