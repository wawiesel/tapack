
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
  IF( ABS(list(i)-val)<=tol )THEN
   loc = i
   RETURN
  END IF
 END DO

END IF