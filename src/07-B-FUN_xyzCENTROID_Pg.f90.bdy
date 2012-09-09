!collapse polygon vertices
Pg_local    = xyzCOLLAPSE_Pg( N , Pg , M32_transform )

!collapse centroid
P_centroid  = xyCENTROID_Pg( N , Pg , SAREA )

!return centroid back to R^3 with expansion procedure
xyzP = xyEXPAND_P( P_centroid , M32_transform )
