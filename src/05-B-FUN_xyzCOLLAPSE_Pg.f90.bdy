!collapse each vertex to R^2 via COLLAPSExyz_P procedure
DO i = 1,N 
 xyPg(:,i) = xyzCOLLAPSE_P(  Pg(:,i) , M32_transform )
END DO
