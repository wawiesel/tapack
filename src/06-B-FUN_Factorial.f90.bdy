IF( x<=MAX )THEN
 y = REAL( FactorialInt(x) , KIND_R )
ELSE
 y = REAL( FactorialInt(MAX) , KIND_R )
 DO j = MAX_I4 + 1_KIND_I , x 
  y = y*REAL(j,KIND_R)
 END DO
ENDIF