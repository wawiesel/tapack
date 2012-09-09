CALL RANDOM_NUMBER(y)
y = (0.5_KIND_R - y)
IF( PRESENT(f) )THEN
 y = (1._KIND_R + y*f)*x
ELSE
 y = x + y*5._KIND_R*EPSILON(1._KIND_R)
END IF