CALL RANDOM_NUMBER(x) 

IF( PRESENT(l) )THEN
 LB = l(1)
 UB = l(2)
 x = LB + x*(UB-LB)
ENDIF