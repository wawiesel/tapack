IF (.not.Allocated(S)) THEN
   IsOk = .FALSE.
   RETURN
END IF
IF ( S % N == size ( S % IA ) - 1 .AND. size (S % A) == size (S%JA)) THEN
   IsOk = .TRUE.
ELSE
   IsOk = .FALSE.
   RETURN
END IF
has_col = .FALSE.
DO i = 1, S % N
   !
   !  Check for rows sorted by increasing order
   !
   IF ( S % IA (i) > S % IA (i+1) ) THEN
      IsOk = .FALSE.
      EXIT
   END IF
   !
   !  Check for coloumn indices out of range
   !
   IF ( ANY ( S % JA ( S % IA(i) : S % IA(i+1)-1 ) < 1) .OR. &
        ANY ( S % JA ( S % IA(i) : S % IA(i+1)-1 ) > S % N ) ) THEN
      IsOk = .FALSE.
      EXIT
   END IF
   !
   !  Check for multiple coloumn indices
   !
   DO k = S % IA(i), S % IA(i+1)-1
      IF (.NOT.has_col ( S % JA (k) )) THEN
         has_col ( S % JA (k) ) = .TRUE.
      ELSE
         IsOk = .FALSE.
      END IF
   END DO
   IF (.NOT.IsOk) EXIT
   has_col ( S % JA (S % IA(i) : S % IA(i+1)-1) ) = .FALSE.
END DO
