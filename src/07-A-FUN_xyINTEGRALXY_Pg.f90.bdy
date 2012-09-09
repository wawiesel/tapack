!! Error condition
IF( N<3 )THEN
 INTEGRALXY = Error(INTEGRALXY)
 RETURN
END IF

!! Initialize.
INTEGRALXY = c_0


im1 = N
DO i = 1, n
 
 INTEGRALXY = INTEGRALXY + ( &
      Pg(2,i) * ( c_3 * Pg(1,i)**2 + c_2 * Pg(1,i) * Pg(1,im1) &
      + Pg(1,im1)**2 ) + Pg(2,im1) * ( Pg(1,i)**2 + c_2 * Pg(1,i) * Pg(1,im1) &
      + c_3 * Pg(1,im1)**2 ) ) * ( Pg(2,i) - Pg(2,im1) )

 im1 = i

END DO

INTEGRALXY = c_1_by_24 * INTEGRALXY
