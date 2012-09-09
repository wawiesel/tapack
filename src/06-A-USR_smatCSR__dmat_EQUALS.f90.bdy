nrow = S % N
ncol = MAXVAL( S % JA )
A = ZERO
DO i = 1,nrow
   DO k = S % IA (i), S % IA(i+1)-1
      A(S % JA(k),i) = S % A (k)
   END DO
END DO
A = TRANSPOSE(A)
