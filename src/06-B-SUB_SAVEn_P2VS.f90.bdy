IF( ASSOCIATED(P) )THEN
 m = SIZE(P,1)
 n = SIZE(P,2)
ELSE
 m = 0
 n = 0 
END IF


IF( Unformatted_ )THEN

 WRITE(Unit)m
 WRITE(Unit)n
 IF( n>0 )THEN
  DO n=LBOUND(P,2),UBOUND(P,2)
   DO m=LBOUND(P,1),UBOUND(P,1) 
    WRITE(Unit)LEN(P(m,n))
	WRITE(Unit)STR(P(m,n))
   END DO
  END DO
 END IF
 
ELSE
 
 WRITE(Unit,*)m
 WRITE(Unit,*)n
 IF( n>0 )THEN
  DO n=LBOUND(P,2),UBOUND(P,2)
   DO m=LBOUND(P,1),UBOUND(P,1) 
    WRITE(Unit,*)LEN(P(m,n))
	CALL PUT(Unit=Unit,String=P(m,n))
   END DO
  END DO
 END IF

END IF