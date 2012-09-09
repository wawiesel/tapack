IF( PRESENT(tol) )THEN
 tol_ = tol
ELSE
 tol_ = 0._KIND_R
END IF

IF( PRESENT(DIST_Ry) )THEN
 DIST_Ry = HUGE(DIST_Ry)
ENDIF

IF( PRESENT(FRAC_Ls) )THEN
 FRAC_Ls = HUGE(FRAC_Ls)
ENDIF

Ln(:,1) = Ls(:,1)
Ln(:,2) = Ls(:,2)-Ls(:,1) 

INTERSECT = xyINTERSECT_LnRy( Ln , Ry , SDIST_Ln=FRAC , DIST_Ry=DIST )

IF( .NOT.INTERSECT )RETURN

!approximate condition for intersection of line segment's fuzzy endpoints
IF( IsApprox(abs(FRAC),tol_) )THEN
 INTERSECT = .TRUE.

ELSE IF( IsApprox(abs(FRAC-1._KIND_R),tol_) )THEN
 INTERSECT = .TRUE.

!outside endpoints
ELSE IF( 0._KIND_R>FRAC .OR. FRAC>1._KIND_R )THEN
 INTERSECT = .FALSE.
 RETURN

!behind ray
ELSE IF( DIST<=0._KIND_R )THEN
 INTERSECT = .FALSE.
 RETURN
ENDIF

IF( PRESENT(DIST_Ry) )THEN
 DIST_Ry = DIST
ENDIF

IF( PRESENT(FRAC_Ls) )THEN
 FRAC_Ls = FRAC
ENDIF

IF( PRESENT(P_intersect) )THEN
 P_intersect = Ry(:,1) + DIST*Ry(:,2)
ENDIF