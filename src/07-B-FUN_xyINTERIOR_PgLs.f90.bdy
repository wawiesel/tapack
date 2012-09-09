!whether or not to include the polynomial edges for the first point
IF( PRESENT(IncludeEnds) )THEN
 IncludeEdges1 = IncludeEnds(1)
 IncludeEdges2 = IncludeEnds(2)
ELSE
 IncludeEdges1 = DEFAULT_IncludeEnds
 IncludeEdges2 = DEFAULT_IncludeEnds
END IF

!test the first point to see if interior
INTERIOR = xyINTERIOR_PgP( N , Pg , Ls(:,1) , P_centroid , &
  KEY=KEY , tol=tol , IncludeEdges = IncludeEdges1 )

IF( INTERIOR )RETURN

!test the second point to see if interior
INTERIOR = xyINTERIOR_PgP( N , Pg , Ls(:,2) , P_centroid , &
  KEY=KEY , tol=tol , IncludeEdges = IncludeEdges2 )

IF( INTERIOR )RETURN


!now we must look for intersections
P2 = Pg(:,N)

!test each line segment face
DO k=1,N
 
 !set P1
 P1 = Pg(:,k)
 
 !check interior
 INTERIOR = xyINTERSECT_LsLs( Ls , RESHAPE( (/P1,P2/) , (/2,2/) ) , &
   KEY=KEY , IncludeEnds_a = (/.TRUE.,.FALSE./) , &
             IncludeEnds_b = (/IncludeEdges1,IncludeEdges2/) )

 !go ahead and kick out if we can
 IF( INTERIOR )RETURN
 
 !update new P2
 P2 = P1

END DO
