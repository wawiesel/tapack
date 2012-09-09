
IF( Unformatted_ )THEN
 
 READ(Unit)n
 IF( n>0 )THEN
  ALLOCATE( P(n) )
  DO n=LBOUND(P,1),UBOUND(P,1)
   READ(Unit)NChars
   DO IChars=1,NChars
    READ(Unit)char
    P(n) = P(n)//char
   END DO
  END DO
 END IF

ELSE
 
 READ(Unit,*)n
 IF( n>0 )THEN
  ALLOCATE( P(n) )
  DO n=LBOUND(P,1),UBOUND(P,1)
   READ(Unit,*)Nchars
   CALL GET(Unit=Unit,String=P(n),maxlen=NChars)
  END DO
 END IF

END IF
