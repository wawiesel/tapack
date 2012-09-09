  IF ( i == j ) THEN
     S % A ( i ) = S % A ( i ) + val
  ELSE
     DO k = S % JA (i), S % JA ( i + 1 ) - 1
        if ( S % JA ( k ) == j ) EXIT
     END DO
     IF ( k < S % JA ( i + 1 ) ) THEN
        S % A ( k ) = S % A ( k ) + val
     ELSE
        STOP "Setting value violating TYPE_smatMSR matrix sparsity pattern."
     ENDIF
  ENDIF