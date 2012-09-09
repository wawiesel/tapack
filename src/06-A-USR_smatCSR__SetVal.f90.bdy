DO k = S % IA ( i ), S % IA ( i + 1 ) - 1
   if ( S % JA ( k ) == j ) EXIT
END DO
IF ( k < S % IA ( i + 1 ) ) THEN
   S % A ( k ) = val
ELSE
   STOP "Setting value violating CSR matrix sparsity pattern."
END IF