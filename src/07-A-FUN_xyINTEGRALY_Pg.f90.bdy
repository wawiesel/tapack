!! Error condition
IF( N<3 )THEN
 INTEGRALY = Error(INTEGRALY)
 RETURN
END IF

!! Initialize.
INTEGRALY = c_0


im1 = N
DO i=1,N

 INTEGRALY = INTEGRALY + ( Pg(2,i)**2 + Pg(2,i) * Pg(2,im1) + Pg(2,im1)**2 ) &
      * ( Pg(1,i) - Pg(1,im1) )

 im1 = i

END DO


INTEGRALY = c_1_by_6 * INTEGRALY

