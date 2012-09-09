SELECT CASE(NDim)
 !one dimension
 CASE(CGE_nd1)
 
 !two dimensions
 CASE(CGE_nd2) 
  SELECT CASE(Coord)
   CASE(CGE_Rec) ; ALLOCATE( Pn(xySHAPE_Pn(1)) )
   CASE(CGE_Sph)
   CASE(CGE_Cyl)
  END SELECT

 !three dimensions
 CASE(CGE_nd3)

END SELECT