!get convex hull ordering
order_cvh = xyCONVEXHULL_Px( N , Px )

!reorder the verts
xyPg(1,:) = Reorder( Px(1,:) , order_cvh , SIDE="R" )
xyPg(2,:) = Reorder( Px(2,:) , order_cvh , SIDE="R" )

!get standardized reordering
order_Pg = xyORDER_Pg( N , xyPg )

xyPg(1,:) = Reorder( xyPg(1,:) , order_Pg , SIDE="R" )
xyPg(2,:) = Reorder( xyPg(2,:) , order_Pg , SIDE="R" )

!handle final ordering output
order = Reorder( order_cvh , order_Pg  , SIDE="R" )
