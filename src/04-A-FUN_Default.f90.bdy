IF( PRESENT(OVERRIDE) )THEN
 IF( OVERRIDE )THEN
  VAL = DEFAULT_VAL
  RETURN
 END IF
END IF

IF( PRESENT(OPTIONAL_VAL) )THEN
 VAL = OPTIONAL_VAL
ELSE
 VAL = DEFAULT_VAL
END IF