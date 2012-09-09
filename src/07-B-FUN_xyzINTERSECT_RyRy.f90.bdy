!! This system of two rays is described by the following parametric equations
!
!!   L1(t1) = Ry_a(:,1) + Ry_a(:,2)*t1
!!   L2(t2) = Ry_b(:,1) + Ry_b(:,2)*t2
!
!! We wish to find the intersection point, t1 & t2 which satisfy L1(t1)=L2(t2).


Ry_c(:,2) = xyzCROSS_VV( Ry_a(:,2) , Ry_b(:,2) )
DIST_c     = xyzNORMSQRD_V( Ry_c(:,2) )

!return if only intersection existance is desired
IF( DIST_c<=c_0 )THEN
 INTERSECT = .FALSE.
 RETURN
ELSE
 INTERSECT = .TRUE.
END IF

!return if USEr doesn't care about distances
IF( .NOT.( PRESENT(DIST_a).OR.PRESENT(DIST_b).OR.PRESENT(P_intersect) ) )RETURN

!assemble matrix which describes system of 2 equations
!the first and third columns are identical for either distance
M33(:,1) = Ry_b(:,1)-Ry_a(:,1) 
M33(:,3) = Ry_c(:,2)

IF( PRESENT(DIST_a) )THEN
 M33(:,2) = Ry_b(:,2) 
 DIST_a = xyzDETERMINANT(M33) / DIST_c
END IF

IF( PRESENT(DIST_b) )THEN
 M33(:,2) = Ry_a(:,2)
 DIST_b = xyzDETERMINANT(M33) / DIST_c
END IF

IF( PRESENT(P_intersect) )THEN

 IF( PRESENT(DIST_a) )THEN
  P_intersect = Ry_a(:,1) + Ry_a(:,2)*DIST_a
 
 ELSEIF( PRESENT(DIST_b) )THEN
  P_intersect = Ry_b(:,1) + Ry_b(:,2)*DIST_b
 
 ELSE
  M33(:,2) = Ry_b(:,2) 
  DIST_c = xyzDETERMINANT(M33) / DIST_c
  P_intersect = Ry_a(:,1) + Ry_a(:,2)*DIST_c
 
 END IF

END IF 

!If the lines do not intersect, DIST_a and DIST_b mark the points of closest approach