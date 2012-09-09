IF( PRESENT(tol_a) )THEN
 tol_a_ = tol_a
ELSE
 tol_a_ = 0._KIND_R
END IF

IF( PRESENT(tol_b) )THEN
 tol_b_ = tol_b
ELSE
 tol_b_ = 0._KIND_R
END IF

IF( PRESENT(IncludeEnds_a) )THEN
 IncludeEnds_a_ = IncludeEnds_a
ELSE
 IncludeEnds_a_ = DEFAULT_IncludeEnds
END IF

IF( PRESENT(IncludeEnds_b) )THEN
 IncludeEnds_b_ = IncludeEnds_b
ELSE
 IncludeEnds_b_ = DEFAULT_IncludeEnds
END IF

IF( PRESENT(key) )THEN
 key = 0 
END IF

IF( PRESENT(FRAC_a) )THEN
 FRAC_a = HUGE(FRAC_a)
ENDIF

IF( PRESENT(FRAC_b) )THEN
 FRAC_b = HUGE(FRAC_b)
ENDIF

Ln_a = xyLINESEGMENT_PP( Ls_a(:,1) , Ls_a(:,2) )
Ln_b = xyLINESEGMENT_PP( Ls_b(:,1) , Ls_b(:,2) )

!call the line-line intersection kernel
INTERSECT = xyINTERSECT_LnLn( Ln_a , Ln_b , SDIST_a=FRAC_a_ , SDIST_b=FRAC_b_ )

!simple return on no intersection of two lines
IF( .NOT.INTERSECT )RETURN

!if we are at the left side of the first line
IF( IsApprox(abs(FRAC_a_),tol_a_) )THEN
 INTERSECT = MERGE( .TRUE. , .FALSE. , IncludeEnds_a_(1) )
 IF( PRESENT(key) )THEN
  key       = key + MERGE( 1      , -1      , INTERSECT )
 END IF

!if we are at the right side of the first line
ELSE IF( IsApprox(abs(FRAC_a_-1._KIND_R),tol_a_) )THEN
 INTERSECT = MERGE( .TRUE. , .FALSE. , IncludeEnds_a_(2) )
 IF( PRESENT(key) )THEN
  key       = key + MERGE( 2      , -2      , INTERSECT )
 END IF

!if we are outside of the interval for the first line
ELSE IF( FRAC_a_<0._KIND_R .OR. FRAC_a_>1._KIND_R )THEN
 INTERSECT = .FALSE.
 RETURN

!if we are in the middle
ELSE
 INTERSECT = .TRUE. 
END IF


!if we are at the left side of the second line
IF( IsApprox(FRAC_b_,tol_b_) )THEN
 INTERSECT = MERGE( .TRUE. , .FALSE. , IncludeEnds_b_(1) )
 IF( PRESENT(key) )THEN
  key       = key + MERGE( 10     , -10      , INTERSECT )
 END IF

!if we are at the right side of the second line
ELSE IF( IsApprox(FRAC_b_-1._KIND_R,tol_b_) )THEN
 INTERSECT = MERGE( .TRUE. , .FALSE. , IncludeEnds_b_(2) )
 IF( PRESENT(key) )THEN
  key       = key + MERGE( 20     , -20      , INTERSECT )
 END IF

!if we are outside of the interval for the second line
ELSE IF( FRAC_b_<0._KIND_R .OR. FRAC_b_>1._KIND_R )THEN
 INTERSECT = .FALSE.
 RETURN

!if we are in the middle
ELSE
 INTERSECT = .TRUE. 
END IF

IF( PRESENT(FRAC_a) )THEN
 FRAC_a = FRAC_a_
END IF

IF( PRESENT(FRAC_b) )THEN
 FRAC_b = FRAC_b_
END IF

IF( PRESENT(P_intersect) )THEN
 P_intersect = Ln_a(:,1) + FRAC_a_ * Ln_a(:,2)
END IF