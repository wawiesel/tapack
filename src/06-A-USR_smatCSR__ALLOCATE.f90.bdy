IF( N <= 0 )THEN
 !allocate_CSR: matrix must have positive size
 RETURN
END IF
IF (PRESENT(NZMAX)) THEN
   SIZE = MAX(NZMAX,1)      ! One element is the least possible
ELSE
   SIZE = MAX(N,Block_Size) ! Default memory, never allow Block_Size less than N 
END IF
ALLOCATE ( A % A (SIZE), A % JA (SIZE), A % IA (N+1) )
!
!  IMPORTANT: Initially all row start pointers point to the first element
!             of JA.  This defines all rows to be of length zero, and makes
!             row insertion easier.
A % IA = 1
A % N = N
IF( PRESENT(InitDiag_dvec) )THEN
 FORALL(i=1:N) 
  A%IA(i) = i
  A%JA(i) = i
  A% A(i) = InitDiag_dvec(i)
 END FORALL
ELSE IF( PRESENT(InitDiag_scalar) )THEN
 FORALL(i=1:N)
  A%IA(i) = i
  A%JA(i) = i
  A% A(i) = InitDiag_scalar
 END FORALL
END IF