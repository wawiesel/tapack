!determine vector from ray base to sphere center
P0 = xyzVECTOR_PP( Ry(1:3) , Sp(1:3) )

!get distance squared, d^2 = ||P0||
d_sqr = xyzNORMSQRD_V( P0 )

!ray is interior to sphere if distance squared is less than radius squared
IF( d_sqr<Sp(4) )THEN
 
 INTERSECT = .TRUE.
 
 !determine intersection point
 IF( PRESENT(DIST) .OR. PRESENT(P_intersect) )THEN
  !get closest approach along ray
  d = xyzDOT_VV( P0 , Ry(4:6) )

  !calculate half chord distance squared
  d_hcsqr = Sp(4) - d_sqr + d

  !calculate distance to intersection from inside sphere
  d = d + SQRT(d_hcsqr)
 END IF
 
!ray is exterior to sphere
ELSE
 !get closest approach along ray
 d = xyzDOT_VV( P0 , Ry(4:6) )
 
 !intersection only if d_hcsqr is greater than or equal to zero
 IF( d<c_0 )THEN
  INTERSECT = .FALSE.
  RETURN

 ELSE
  !calculate half chord distance squared
  d_hcsqr = Sp(4) - d_sqr + d
  
  IF( d_hcsqr<c_0 )THEN
   INTERSECT = .FALSE.
   RETURN

  ELSE
   INTERSECT = .TRUE. 
   IF( PRESENT(DIST) .OR. PRESENT(P_intersect) )THEN
    !calculate distance to intersection
    d = d + SQRT(d_hcsqr)
   END IF
  
  END IF

 END IF

END IF

!return distance to intersection
IF( PRESENT(DIST) )THEN
 DIST = d
END IF
 
!return intersection point
IF( PRESENT(P_intersect) )THEN
 P_intersect = Ry(1:3) + d * Ry(4:6)
END IF
