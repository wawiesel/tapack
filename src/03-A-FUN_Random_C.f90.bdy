CALL RANDOM_NUMBER(rand) 

LB = l(1)
UB = l(2)

x = REAL (LB) + rand(1)*REAL((UB-LB),KIND_C) + &
    CMPLX(LB) + rand(2)*REAL((UB-LB),KIND_C) 

