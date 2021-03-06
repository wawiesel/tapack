!get dot product of ray direction and plane normal
s0 = xyDOT_VV( Pn(1:2) , Ry(:,2) )

!if s0 is zero then the plane and ray are parallel
IF( s0==c_0 )THEN
 INTERSECT = .FALSE.

!otherwise, there will be an intersection with the line
ELSE
 !calculate the distance from the plane to the base of the ray
 d = xySDIST_PnP( Pn , Ry(:,1) )
 
 !no intersection with the ray
 IF( d*s0>c_0 )THEN
  INTERSECT = .FALSE. 

 !intersection exists
 ELSE
 
  !distance to the plane is shortest distance divided by dot product
  d = -d/s0
 
  INTERSECT = .TRUE.
 
  !return distance to intersection point
  IF( PRESENT(DIST) )THEN
   DIST = d
  END IF

  !calculate intersection point if desired
  IF( PRESENT(P_intersect) )THEN
   P_intersect = Ry(:,1) + d * Ry(:,2)
  END IF

 END IF

END IF