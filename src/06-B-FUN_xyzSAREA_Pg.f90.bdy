!collapse R^3 polygon into R^2 polygon
Pg_local  = xyzCOLLAPSE_Pg( N , Pg , M32_transform )
 
!calculate signed area of polygon using R^2 representation
sAREA = xySAREA_Pg( N , Pg_local )