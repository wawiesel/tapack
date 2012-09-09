!! Error condition
IF( N<3 )THEN
 INTEGRALYY = Error(INTEGRALYY)
 RETURN
END IF

!! Initialize.
INTEGRALYY = c_0


im1 = N
DO i = 1, n
 
 INTEGRALYY = INTEGRALYY - ( Pg(2,i)**3 + Pg(2,i)**2 * Pg(2,im1) &
    + Pg(2,i) * Pg(2,im1)**2 + Pg(2,im1)**3 ) * ( Pg(1,i) - Pg(1,im1) )

 im1 = i

END DO

INTEGRALYY = c_1_by_12 * INTEGRALYY

