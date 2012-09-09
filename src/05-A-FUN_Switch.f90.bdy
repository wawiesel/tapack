IF( Conditional )THEN
 VAL = A
ELSE
 IF( PRESENT(B) )THEN
  VAL = B
 ELSE
  VAL = Error(VAL)
 END IF
END IF
