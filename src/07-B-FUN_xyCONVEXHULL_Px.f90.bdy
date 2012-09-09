!**STAGE 1: Sort**

!set polygon points
imin = MINLOC( Px(1,:) , 1 )
imax = MAXLOC( Px(1,:) , 1 )

!determine bisector to rightmost point
U_bisector = xyDIRECTION_PP( Px(:,imin) , Px(:,imax) )

!first move the origin to the minimum point
Px_local = xyTRANSLATE_Px( N , Px , -Px(:,imin) )

!get the angle from the x-axis to the bisector
t = xyANGLE_UU( (/c_1,c_0/) , U_bisector )

!rotate all points such that the two points on the bisector
!are the new x-axis
Px_local = xyROTATE_Px( N , Px_local , -t )


!**STAGE 2: Determine Upper and Lower Chain Masks**
!determine the upper chain based on y>0 and both extreme points
!are always included
mask_upper = Px_local(2,:)>c_0
mask_upper(imax) = .TRUE.
mask_upper(imin) = .TRUE.

!determine the lower chain from remaining points
mask_lower = .NOT.mask_upper
mask_lower(imax) = .TRUE.
mask_lower(imin) = .TRUE.


!**STAGE 3: Traverse Upper Chain**

!count the number of points on the upper chain
N_upper = COUNT( mask_upper )

!allocate and set the points of the upper chain into Px_ws
!note that we traverse the lists backwards because we need
!the first point of the chain to be i=N
ALLOCATE( Px_ws(1:2,1:N_upper) , ilist(1:N_upper) , jlist(1:N_upper) )
j = 0
DO i=1,N
 IF( mask_upper(i) )THEN
  j = j + 1
  Px_ws(:,j) = Px(:,i)
  ilist(j) = i
  IF( i==imin )jmin=j
  IF( i==imax )jmax=j
 END IF
END DO

!initialize with ii=0
ii = 0

!add the rest of the points
jlist = xyCOUNTERCLOCKWISE_Cn( N_upper , Px_ws , base=jmax , anchor=jmin , ReverseOrder=.TRUE. )
DO j=1,N_upper
 k = jlist(j)
 IF( k==0 )CYCLE
 ii = ii + 1
 order_cvh(ii) = ilist(k)
END DO

!deallocate the the work spaces
DEALLOCATE( Px_ws , ilist , jlist )

!**STAGE 4: Traverse Lower Chain**

!count the number of points on the lower chain
N_lower = COUNT( mask_lower )

!allocate and set the points of the lower chain into Px_ws
ALLOCATE( Px_ws(1:2,1:N_lower) , ilist(1:N_lower) , jlist(1:N_lower) )
j = 0
DO i=1,N
 IF( mask_lower(i) )THEN
  j = j + 1
  Px_ws(:,j) = Px(:,i)
  ilist(j) = i
  IF( i==imin )jmin=j
  IF( i==imax )jmax=j
 END IF
END DO

!add the rest of the points to complete the convex hull
jlist = xyCOUNTERCLOCKWISE_Cn( N_lower , Px_ws , base=jmin , anchor=jmax , ReverseOrder=.FALSE. ) 
DO j=1,N_lower
 k = jlist(j)
 IF( k==0 )CYCLE
 ii = ii + 1
 order_cvh(ii) = ilist(k)
END DO

order_cvh(ii+1:) = 0
j = ii
DO i=1,N
 IF( .NOT.ANY(order_cvh(1:ii)==(/(i,k=1,ii)/)) )THEN
  j = j + 1
  order_cvh(j) = -i
 END IF
END DO
!deallocate the work spaces
DEALLOCATE( Px_ws , ilist , jlist )