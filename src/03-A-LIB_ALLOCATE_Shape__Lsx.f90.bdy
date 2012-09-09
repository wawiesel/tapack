SELECT CASE(NDim)
 !one dimension
 CASE(CGE_nd1)
 
 !two dimensions
 CASE(CGE_nd2) 
  SELECT CASE(Coord)
   CASE(CGE_rec) ; ALLOCATE( Lsx(xySHAPE_Ls(1),xySHAPE_Ls(2),N) )
   CASE(CGE_sph)
   CASE(CGE_cyl)
  END SELECT

 !three dimensions
 CASE(CGE_nd3)

END SELECT