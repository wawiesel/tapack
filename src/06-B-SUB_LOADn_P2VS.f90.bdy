
IF( Unformatted_ )THEN
 
 READ(Unit)m
 READ(Unit)n
 IF( n>0 )THEN
  ALLOCATE( P(m,n) )
  DO n=LBOUND(P,2),UBOUND(P,2)
   DO m=LBOUND(P,1),UBOUND(P,1) 
    READ(Unit)NChars
    DO IChars=1,NChars
     READ(Unit)char
     P(m,n) = P(m,n)//char
    END DO
   END DO
  END DO
 END IF

ELSE
 
 READ(Unit,*)m
 READ(Unit,*)n
 IF( n>0 )THEN
  ALLOCATE( P(m,n) )
  DO n=LBOUND(P,2),UBOUND(P,2)
   DO m=LBOUND(P,1),UBOUND(P,1) 
    READ(Unit,*)Nchars
    CALL GET(Unit=Unit,String=P(m,n),maxlen=NChars)
   END DO
  END DO
 END IF

END IF



DO n=LBOUND(P,2),UBOUND(P,2)
 DO m=LBOUND(P,1),UBOUND(P,1) 
  
 END DO
END DO