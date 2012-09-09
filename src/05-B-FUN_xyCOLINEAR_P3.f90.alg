n1 = P3(1,2) - P3(1,1)
d1 = P3(2,2) - P3(2,1)
n2 = P3(1,3) - P3(1,1)
d2 = P3(2,3) - P3(2,1)

!Now using "cross multiplication" we have:
diff = ABS(n1*d2-n2*d1)

!should be 0 or less than tolerance, if we have one
tol_ = DEFAULT( DEFAULT_tol , tol )

COLINEAR = (diff<=tol_)

!return an error
IF( PRESENT(err) )THEN
 err = diff
END IF
