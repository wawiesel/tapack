
IF( Unformatted_ )THEN
 
 READ(Unit)m
 READ(Unit)n
 IF( m>0 .AND. n>0 )THEN
  ALLOCATE( P(m,n) )
  DO n=LBOUND(P,2),UBOUND(P,2)
   DO m=LBOUND(P,1),UBOUND(P,1) 
    READ(Unit)P(m,n)
   END DO
  END DO
 END IF

ELSE
 
 READ(Unit,*)m
 READ(Unit,*)n
 IF( m>0 .AND. n>0 )THEN
  ALLOCATE( P(m,n) )
  DO n=LBOUND(P,2),UBOUND(P,2)
   DO m=LBOUND(P,1),UBOUND(P,1) 
    READ(Unit,100,IOSTAT=ios)P(m,n)
    IF( ios/=0 )THEN
     P(m,n) = Error( P(m,n) )
    END IF
   END DO
  END DO
 END IF

END IF

