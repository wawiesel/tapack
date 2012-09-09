!get number of dimensions
Nd = SIZE(X,1)

!pack operation
DO i=1,Nd
 Y(i,:) = PACK( X(i,:) , MASK )
END DO

