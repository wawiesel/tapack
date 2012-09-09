!kick out with false if sizes are unequal
IF( SIZE(A,1)/=SIZE(B,1) .OR. SIZE(A,2)/=SIZE(B,2) )THEN
 IsApprox=.FALSE.
 RETURN
END IF

!optional handling
IF( PRESENT(tol) )THEN
 !set local tolerance
 tol_ = tol
ELSE
 
 !get average
 avg = 0.5_KIND_R*SUM(A+B)/REAL(SIZE(A),KIND_R)

 !get local tolerance
 IF( PRESENT(reltol) )THEN 
  tol_ = reltol*ABS(avg)
 ELSE
  tol_ = SQRT(MAX(EPSILON(A),EPSILON(B)))
 END IF

END IF

!statement
IsApprox = ALL( ABS(A-B)<=tol_ )
