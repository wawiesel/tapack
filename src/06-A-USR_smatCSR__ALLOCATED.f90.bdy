Allocated = ASSOCIATED ( A % A )
IF ((Allocated.AND.(.NOT.ASSOCIATED(A % IA).OR..NOT.ASSOCIATED(A % JA))) .OR.&
     & (.NOT.Allocated.AND.(ASSOCIATED(A % IA).OR.ASSOCIATED(A % JA))) .OR.&
     & (.NOT.ASSOCIATED (A % IA).AND.ASSOCIATED(A % JA)) .OR.&
     & (ASSOCIATED(A % IA).AND..NOT.ASSOCIATED(A % JA)) .OR.&
     & (Allocated.AND.A % N <=0))THEN
 !illegal associacion in CSR, probably not NULLIFIED
 RETURN
END IF

