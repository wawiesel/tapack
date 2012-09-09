!collapse R^3 quadrilateral <Qg> into R^2 quadrilateral <Qd_local>
Qg_local  = xyzCOLLAPSE_Pg( N_ , Qg , M32_transform )
 
!calculate signed area of quadrilateral using R^2 representation
sAREA = xySAREA_Qg( Qg_local )
