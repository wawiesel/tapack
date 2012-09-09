!calculate normalization factor
norm = xyzNORM_V( V )

IF( norm==c_0 )THEN
 xyzPn = c_0
ELSE
 !define normal vector to plane and constant s
 xyzPn = (/ V , -xyzDOT_VV(V,P) /) / norm
END IF