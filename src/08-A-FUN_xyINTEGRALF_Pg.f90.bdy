IF( PRESENT(Area) )THEN
 xyA = Area
ELSE
 xyA = xySAREA_Pg( N , Pg )
END IF

IF( PRESENT(P_centroid) )THEN
 xyC = P_centroid
ELSE
 xyC = xyCENTROID_Pg( N , Pg , Area=xyA )
END IF

order_Trx = xyTRIANGULATE_Pg( N , Pg , P_centroid=xyC )

!assemble the triangles
DO m=1,N
 
 DO l=1,3
  IF( order_Trx(l,m)==0 )THEN
   Trx(:,l,m) = xyC
  ELSE
   Trx(:,l,m) = Pg(:,order_Trx(l,m))
  END IF
 END DO

END DO

INTEGRALF = xyINTEGRALF_Trx( f , N , Trx , &
  reltol , abstol , &
  nmin,nmax , err,num_eval,ierr )