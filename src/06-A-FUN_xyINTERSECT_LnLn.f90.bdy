IF( PRESENT(SDIST_a) )THEN
 SDIST_a = HUGE(SDIST_a)
END IF

IF( PRESENT(SDIST_b) )THEN
 SDIST_b = HUGE(SDIST_b)
END IF

U_aperp = xyPERPCCW_V( Ln_a(:,2) )
U_bperp = xyPERPCCW_V( Ln_b(:,2) )

P_c = Ln_b(:,1) - Ln_a(:,1)

s3 = xyDOT_VV( Ln_a(:,2) , U_bperp )
!should just be negative of s3, s4=-s3
s4 = xyDOT_VV( Ln_b(:,2) , U_aperp )

!set intersect value
IF( s3==0._KIND_R )THEN
 INTERSECT = .FALSE.
 RETURN
ELSE
 INTERSECT = .TRUE.
END IF

IF( PRESENT(SDIST_a).AND.PRESENT(SDIST_b) )THEN
 SDIST_a = xyDOT_VV(  P_c , U_bperp )/s3
 SDIST_b = xyDOT_VV( -P_c , U_aperp )/s4
ELSE IF( PRESENT(SDIST_a) )THEN
 !here we only divide once so the predivision step is pointless
 SDIST_a = xyDOT_VV(  P_c , U_bperp )/s3
ELSE IF( PRESENT(SDIST_b) )THEN
 !here we only divide once so the predivision step is pointless
 SDIST_b = xyDOT_VV( -P_c , U_aperp )/s4
END IF

IF( PRESENT(P_intersect) )THEN
 IF( PRESENT(SDIST_a) )THEN
  P_intersect = Ln_a(:,1) + SDIST_a*Ln_a(:,2)
 ELSE IF( PRESENT(SDIST_b) )THEN
  P_intersect = Ln_b(:,1) + SDIST_b*Ln_b(:,2)
 ELSE
  s3 = xyDOT_VV( P_c , U_aperp )/s3
  P_intersect = Ln_a(:,1) + s3*Ln_a(:,2)
 END IF
END IF