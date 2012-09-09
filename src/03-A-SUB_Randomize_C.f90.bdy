CALL RANDOM_NUMBER(rand) 

IF( PRESENT(l) )THEN
 LB = l(1)
 UB = l(2)
 rand(1) = REAL (LB) + rand(1)*REAL (UB-LB)
 rand(2) = AIMAG(LB) + rand(2)*AIMAG(UB-LB)
END IF

x = CMPLX( rand(1) , rand(2) )
