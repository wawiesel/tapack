!get dot product of line direction and plane normal
s0 = xyDOT_VV( Pn(1:2) , Ln(:,2) )

!if s0 is zero then the plane and ray are parallel
IF( s0==c_0 )THEN
 INTERSECT = .FALSE.

!otherwise, there will be an intersection with the line
ELSE
 !calculate the distance from the plane to the base of the line
 sd = xySDIST_PnP( Pn , Ln(:,1) )
 
 !distance to the plane is shortest distance divided by dot product
 sd = -sd/s0
 
 INTERSECT = .TRUE.
 
 !return signed distance to intersection point
 IF( PRESENT(SDIST) )THEN
  SDIST = sd
 END IF

 !calculate intersection point if desired
 IF( PRESENT(P_intersect) )THEN
  P_intersect = Ln(:,1) + sd * Ln(:,2)
 END IF

END IF