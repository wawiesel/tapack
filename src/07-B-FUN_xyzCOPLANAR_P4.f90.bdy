!form the plane
Pn = xyzPLANE_P3( P4(:,1:3) )

!check colinearity
COPLANAR = xyzCOPLANAR_PnP( Pn , P4(:,4) )