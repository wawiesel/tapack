order = xyCONTIGUOUS_Lsx( N , Lsx )

DO k=1,N
 IF( order(k)<0 )THEN
  xyPg(:,k) = Lsx(:,2,k)
 ELSE
  xyPg(:,k) = Lsx(:,1,k)
 END IF
END DO