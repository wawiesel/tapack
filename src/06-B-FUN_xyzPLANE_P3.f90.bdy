!get two vectors on the plane
V_ba  = xyzVECTOR_PP( P3(:,1) , P3(:,2) )
V_ca  = xyzVECTOR_PP( P3(:,1) , P3(:,3) )

!USE cross product
V = xyzCROSS_VV( V_ba , V_ca )

!call plane creation for point and perpendicular vector
xyzPn = xyzPLANE_PV( P3(:,1) , V )
