
IF( Unformatted_ )THEN
 
 READ(Unit)n
 IF( n>0 )THEN
  ALLOCATE( P(n) )
  DO n=LBOUND(P,1),UBOUND(P,1)
   READ(Unit)P(n)
  END DO
 END IF

ELSE
 
 READ(Unit,*)n
 IF( n>0 )THEN
  ALLOCATE( P(n) )
  DO n=LBOUND(P,1),UBOUND(P,1)
   READ(Unit,100,IOSTAT=ios)P(n)
   IF( ios/=0 )THEN
    P(n) = Error( P(n) )
   END IF
  END DO
 END IF

END IF
