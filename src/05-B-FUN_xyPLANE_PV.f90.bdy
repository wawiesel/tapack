!calculate normalization factor
norm = xyNORM_V( V )

!define normal vector to plane and constant s
IF( norm==c_0 )THEN
 Pn = c_0
ELSE
 Pn = (/ V , -xyDOT_VV(V,P) /) / norm
END IF