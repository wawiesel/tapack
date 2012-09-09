loc = 0

IF( .NOT.PRESENT(tol) )THEN
 
 DO i=1,SIZE(list)
  IF( list(i)==val )THEN
   loc = i
   RETURN
  END IF
 END DO

ELSE

 DO i=1,SIZE(list)
  s = (list(i)-val)
  IF( REAL(s)**2 + AIMAG(s)**2<=tol**2 )THEN
   loc = i
   RETURN
  END IF
 END DO

END IF