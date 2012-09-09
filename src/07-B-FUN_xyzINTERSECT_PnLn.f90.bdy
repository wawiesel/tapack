!get dot product
s0 = xyzDOT_VV( Pn(1:3) , Ln(4:6) )

!if s0 is zero then the plane and line are parallel
IF( s0==c_0 )THEN
 INTERSECT = .FALSE.
 RETURN

!otherwise, there will be an intersection with the line
ELSE
 INTERSECT = .TRUE.
 
 !calculate distance from plane to point
 d = xyzSDIST_PnP( Pn , Ln(1:3) )
 
 !calculate ratio
 d = d/s0
 
END IF

!return signed distance to intersection point
IF( PRESENT(SDIST) )THEN
 SDIST = d
END IF

!calculate intersection point if desired
IF( PRESENT(P_intersect) )THEN
 P_intersect = Ln(1:3) + d * Ln(4:6)
END IF
