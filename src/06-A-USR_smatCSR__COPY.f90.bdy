nrow = A%N
nzmax = A%IA(nrow+1)-1

IF( UBOUND(S%A,1)>=nzmax .AND. UBOUND(S%IA,1)>=nrow+1 )THEN
 S%N = nrow
 S%IA(1:nrow+1) = A%IA(1:nrow+1)
 S%JA(1:nzmax) = A%JA
 S% A(1:nzmax) = A% A

ELSE     
 CALL DEALLOCATE( S )
 CALL allocate( S, nrow ,nzmax)
 S%IA = A%IA
 DO i = 1, nrow
  kbeg = A%IA(i  )
  kend = A%IA(i+1)-1
  CALL setrow(S,i,A%JA(kbeg:kend),A%A(kbeg:kend))
 END DO

ENDIF