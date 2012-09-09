
!get a ray from the two points
Ry = xyRAY_PP( Ls(:,1) , Ls(:,2) )

INTERSECT = xyINTERSECT_PnRy( Pn , Ry , DIST=DIST_ , P_intersect=P_intersect )

!intersection with ray
IF( INTERSECT )THEN
 
 !no intersection with line segment---distance too far
 IF( DIST_>xyDIST_PP( Ls(:,1) , Ls(:,2) ) )THEN
  INTERSECT = .FALSE.
  IF( PRESENT(P_intersect) )THEN
   P_intersect = ERROR(1._KIND_R)
  END IF
  IF( PRESENT(DIST) )THEN
   DIST = ERROR(1._KIND_R)
  END IF
 
 ELSE
  IF( PRESENT(DIST) )THEN
   DIST = DIST_
  END IF  
 END IF

END IF
