!! Error condition
IF( N<3 )THEN
 INTEGRALXX = Error(INTEGRALXX)
 RETURN
END IF

!! Initialize.
INTEGRALXX = c_0


im1 = N
DO i = 1, n
 
 INTEGRALXX = INTEGRALXX + ( Pg(1,i)**3 + Pg(1,i)**2 * Pg(1,im1) &
  + Pg(1,i) * Pg(1,im1)**2 + Pg(1,im1)**3 ) * ( Pg(2,i) - Pg(2,im1) )

 im1 = i

END DO

INTEGRALXX = c_1_by_12 * INTEGRALXX
