!calculate normalization factor
norm = xyzNORM_V( C(1:3) )

!define normal vector and constant with simple statement
IF( norm==c_0 )THEN
 xyzPn = c_0
ELSE
 xyzPn = C / norm
END IF