!! There is probably a better way, but for now just do
!! it the long way.
order = xyCONTIGUOUS_Lsx( 4 , Lsx )
DO i=1,4
 j = order(i)
 k = abs(j)
 xyQg(:,k) = MERGE( Lsx(:,1,i) , Lsx(:,2,i) , j>0 )
END DO