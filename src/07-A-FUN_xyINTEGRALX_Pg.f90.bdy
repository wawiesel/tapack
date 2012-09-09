!! Error condition
IF( N<3 )THEN
 INTEGRALX = Error(INTEGRALX)
 RETURN
END IF

!! Initialize.
INTEGRALX = c_0


im1 = N
DO i=1,N

 INTEGRALX = INTEGRALX + ( Pg(1,i)**2 + Pg(1,i) * Pg(1,im1) + Pg(1,im1)**2 ) &
      * ( Pg(2,i) - Pg(2,im1) )

 im1 = i

END DO


INTEGRALX = c_1_by_6 * INTEGRALX

