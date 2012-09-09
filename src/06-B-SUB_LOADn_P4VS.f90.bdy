

IF( Unformatted_ )THEN
 
 READ(Unit)k
 READ(Unit)l
 READ(Unit)m
 READ(Unit)n
 IF( k>0 .AND. l>0 .AND. m>0 .AND. n>0 )THEN
  ALLOCATE( P(k,l,m,n) )
  DO n=LBOUND(P,4),UBOUND(P,4)
   DO m=LBOUND(P,3),UBOUND(P,3) 
    DO l=LBOUND(P,2),UBOUND(P,2)
     DO k=LBOUND(P,1),UBOUND(P,1)
      READ(Unit)NChars 
      DO IChars=1,NChars
       READ(Unit)char
       P(k,l,m,n) = P(k,l,m,n)//char
      END DO
     END DO
    END DO
   END DO
  END DO
 END IF

ELSE
 
 READ(Unit,*)k
 READ(Unit,*)l
 READ(Unit,*)m
 READ(Unit,*)n
 IF( k>0 .AND. l>0 .AND. m>0 .AND. n>0 )THEN
  ALLOCATE( P(k,l,m,n) )
  DO n=LBOUND(P,4),UBOUND(P,4)
   DO m=LBOUND(P,3),UBOUND(P,3) 
    DO l=LBOUND(P,2),UBOUND(P,2)
     DO k=LBOUND(P,1),UBOUND(P,1)
      READ(Unit,*)NChars
      CALL GET(Unit=Unit,String=P(k,l,m,n),maxlen=NChars)
     END DO
    END DO
   END DO
  END DO
 END IF

END IF

