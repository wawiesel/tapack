IF( Conditional )THEN
 VAL = A
ELSE
 IF( PRESENT(B) )THEN
  VAL = B
 ELSE
  VAL = .FALSE.
 END IF
END IF
