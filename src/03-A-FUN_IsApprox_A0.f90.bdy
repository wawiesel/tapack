!optional handling
IF( PRESENT(tol) )THEN
 tol_ = tol
ELSE
 !get average
 avg = 0.5_KIND_R*(A+B)

 !get local tolerance
 IF( PRESENT(reltol) )THEN 
  tol_ = reltol*ABS(avg)
 ELSE
  tol_ = SQRT(MAX(EPSILON(A),EPSILON(B)))
 END IF
END IF

!statement
IsApprox = ( ABS(A-B)<=tol_ )
