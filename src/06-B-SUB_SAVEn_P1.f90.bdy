
IF( ASSOCIATED(P) )THEN
 n = SIZE(P,1)
ELSE
 n = 0 
END IF


IF( Unformatted_ )THEN
 
 WRITE(Unit)n
 IF( n>0 )THEN
  DO n=LBOUND(P,1),UBOUND(P,1)
   WRITE(Unit)P(n)
  END DO
 END IF

ELSE
 
 WRITE(Unit,*)n
 IF( n>0 )THEN
  DO n=LBOUND(P,1),UBOUND(P,1)
   WRITE(Unit,100)P(n)
  END DO
 END IF

END IF