!initialize tolerance
IF( PRESENT(tol) )THEN
 tol_ = tol
ELSE
 tol_ = DEFAULT_tol
END IF

!initialize tolerance
IF( PRESENT(IncludeEdges) )THEN
 IncludeEdges_ = IncludeEdges
ELSE
 IncludeEdges_ = DEFAULT_IncludeEdges
END IF

!initialize interior
INTERIOR = .FALSE.


!start doing i->ip = N->1, the weird one
i      = N
ip     = 1
DO

 !calculate the plane of a face
 Pn = xyPLANE_PV( 0.5_KIND_R*(Pg(:,i)+Pg(:,ip)) , xyPERPCCW_V(Pg(:,ip) - Pg(:,i)) )
 
 !make sure it is inward facing
 IF( xySDIST_PnP( Pn , P_centroid)<c_0 )THEN
  Pn = -Pn
 END IF
 
 !calculate distance from plane of face to point of interest
 s  = xySDIST_PnP( Pn , P )
  
 !check to see if the point is behind the plane
 IF( s<-ABS(tol_) )THEN
  IF( PRESENT(KEY) )THEN
   KEY  = 0
  END IF
  RETURN
 END IF

 !now determine KEY based on following
 IF( PRESENT(KEY) )THEN
   
  !if the dot product is (near) zero, then P must be on the infinite line between i and ip
  IF( ABS(s)<=tol_ )THEN
   
   KEY = i
   
   IF( .NOT.IncludeEdges_ )THEN
    KEY = -KEY
    RETURN
   END IF

  END IF

 END IF

 !update i,ip
 i  = ip
 ip = i + 1
 
 !exit when last point is done
 IF( i==N )EXIT
END DO

!if we made it out then it meant the point was interior
INTERIOR = .TRUE.