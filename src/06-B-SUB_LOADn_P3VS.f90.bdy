

IF( Unformatted_ )THEN
 
 READ(Unit)l
 READ(Unit)m
 READ(Unit)n
 IF( l>0 .AND. m>0 .AND. n>0 )THEN
  ALLOCATE( P(l,m,n) )
  DO n=LBOUND(P,3),UBOUND(P,3)
   DO m=LBOUND(P,2),UBOUND(P,2) 
    DO l=LBOUND(P,1),UBOUND(P,1)
     READ(Unit)NChars 
     DO IChars=1,NChars
      READ(Unit)char
      P(l,m,n) = P(l,m,n)//char
     END DO
    END DO
   END DO
  END DO
 END IF

ELSE
 
 READ(Unit,*)l
 READ(Unit,*)m
 READ(Unit,*)n
 IF( l>0 .AND. m>0 .AND. n>0 )THEN
  ALLOCATE( P(l,m,n) )
  DO n=LBOUND(P,3),UBOUND(P,3)
   DO m=LBOUND(P,2),UBOUND(P,2) 
    DO l=LBOUND(P,1),UBOUND(P,1)
     READ(Unit,*)NChars
     CALL GET(Unit=Unit,String=P(l,m,n),maxlen=NChars)
    END DO
   END DO
  END DO
 END IF

END IF

