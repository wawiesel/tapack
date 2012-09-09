CALL RANDOM_NUMBER(rand) 

IF( PRESENT(l) )THEN
 LB = l(1)
 UB = l(2)
ELSE
 LB = 0
 UB = 10
ENDIF

x = LB + FLOOR(rand*(UB-LB))
