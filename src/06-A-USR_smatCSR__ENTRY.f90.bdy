    IF (A % N <= 0) STOP "entry_CSR: matrix not allocated"
    IF (i<1.OR.i>A % N) STOP "entry_CSR: row out range"
    IF (j<1.OR.j>A % N) STOP "entry_CSR: column out range"
    found = .FALSE.
    DO k = A % IA (i), A % IA (i+1) - 1
       IF (A % JA (k) == j) THEN
          found = .TRUE.
          EXIT
       END IF
    END DO
    IF (found) THEN
       ENTRY = A % A (k)
    ELSE
       ENTRY = ZERO
    END IF
