!collapse R^3 triangle (Tr) into R^2 triangle (Tr_local)
Tr_local  = xyzCOLLAPSE_Pg( N_ , Tr , M32_transform )
 
!calculate signed area of triangle using R^2 representation
sAREA = xySAREA_Tr( Tr_local )

