!rotate about the origin using polar coordinates
!! x = r*cos \theta
!! y = r*sin \theta
!and 
!! cosine conversion of cos(a+b) = cos(a)*cos(b) - sin(a)*sin(b)
!! sine   conversion of sin(a+b) = sin(a)*cos(b) + cos(a)*sin(b)

!return becaUSE the origin can't be rotated
IF( ALL(U==c_0) )THEN
 xyU = U
 RETURN
END IF

!for this case of rotating a direction vector (unit vector)
!we don't need to calculate the radius or inverse (both unity)

!NOTE: t = \theta
cos_t = U(1)
sin_t = U(2)

!NOTE: dt = \Delta \theta
cos_dt = cos(dt)
sin_dt = sin(dt)

!NOTE: tp = \theta' = \theta + \Delta \theta
cos_tp = cos_t*cos_dt - sin_t*sin_dt
sin_tp = sin_t*cos_dt + cos_t*sin_dt

!set the rotated point
xyU = (/ cos_tp , sin_tp /)