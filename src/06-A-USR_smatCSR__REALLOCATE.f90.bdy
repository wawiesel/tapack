
    IF (.NOT.ALLOCATED(S))THEN
      !REALLOCATE_CSR: matrix not allocated
      RETURN
    END IF
    N = S % N
    IF (PRESENT(DELTA)) THEN
       NZMAX = MAX(SIZE(S % A) + DELTA, S % IA (N+1) - 1)
    ELSE
       NZMAX = S % IA (N+1) - 1
    END IF
    ALLOCATE ( A (NZMAX), JA (NZMAX) )
    !
    !  The reallocation involves a memory copy
    !
    A (1:S % IA (N+1)-1) = S % A (1:S % IA (N+1)-1)
    JA (1:S % IA (N+1)-1) = S % JA (1:S % IA (N+1)-1)
    DEALLOCATE ( S % A, S % JA )
    S % A => A
    S % JA => JA