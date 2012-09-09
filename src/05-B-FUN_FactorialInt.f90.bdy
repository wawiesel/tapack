IF( x>MAX )THEN
 y = ERROR(y)
ELSE
 y = 1_KIND_I
 DO j=2_KIND_I,x
  y = y*j
 END DO
ENDIF