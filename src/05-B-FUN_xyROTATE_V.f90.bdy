!rotate about the origin using polar coordinates
!! x = r*cos \theta
!! y = r*sin \theta
!and 
!! cosine conversion of cos(a+b) = cos(a)*cos(b) - sin(a)*sin(b)
!! sine   conversion of sin(a+b) = sin(a)*cos(b) + cos(a)*sin(b)

!return becaUSE the origin can't be rotated
IF( ALL(V==c_0) )THEN
 xyV = V
 RETURN
END IF

!determine radius
r = xyNORM_V( V )

!NOTE: VAR_1_by_r = \frac{1}{r}
VAR_1_by_r = c_1/r

!NOTE: t = \theta
cos_t = V(1)*VAR_1_by_r
sin_t = V(2)*VAR_1_by_r

!NOTE: dt = \Delta \theta
cos_dt = cos(dt)
sin_dt = sin(dt)

!NOTE: tp = \theta' = \theta + \Delta \theta
cos_tp = cos_t*cos_dt - sin_t*sin_dt
sin_tp = sin_t*cos_dt + cos_t*sin_dt

!set the rotated point
xyV = (/ r*cos_tp , r*sin_tp /)