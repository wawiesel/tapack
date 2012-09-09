IF( ASSOCIATED(P) )THEN
 l = SIZE(P,1)
 m = SIZE(P,2)
 n = SIZE(P,3)
ELSE
 l = 0
 m = 0
 n = 0 
END IF



IF( Unformatted_ )THEN

 WRITE(Unit)l
 WRITE(Unit)m
 WRITE(Unit)n
 DO n=LBOUND(P,3),UBOUND(P,3)
  DO m=LBOUND(P,2),UBOUND(P,2) 
   DO l=LBOUND(P,1),UBOUND(P,1)
    WRITE(Unit)LEN(P(l,m,n))
	WRITE(Unit)STR(P(l,m,n))
   END DO
  END DO
 END DO
 
ELSE
 
 WRITE(Unit,*)l
 WRITE(Unit,*)m
 WRITE(Unit,*)n
 DO n=LBOUND(P,3),UBOUND(P,3)
  DO m=LBOUND(P,2),UBOUND(P,2) 
   DO l=LBOUND(P,1),UBOUND(P,1)
    WRITE(Unit,*)LEN(P(l,m,n))
	CALL PUT(Unit=Unit,String=P(l,m,n))
   END DO
  END DO
 END DO

END IF