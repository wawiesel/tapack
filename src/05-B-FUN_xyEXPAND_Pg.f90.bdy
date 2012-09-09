!just call the point expand procedure on each point
FORALL( i = 1:N )
 xyzPg(:,i) = xyEXPAND_P( Pg(:,i) , M32_transform )
END FORALL

