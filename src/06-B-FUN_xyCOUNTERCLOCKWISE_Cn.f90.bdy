!create the workspace array of x-values
P_x = Cn(1,:)

!make sure the "anchor point" is the  last point
!make sure the "base point"   is the first point
IF( MERGE( ReverseOrder , .FALSE. , PRESENT(ReverseOrder) ) )THEN
 P_x(base)   = +HUGE(P_x)
 P_x(anchor) = -HUGE(P_x)
ELSE
 P_x(base)   = -HUGE(P_x)
 P_x(anchor) = +HUGE(P_x)
ENDIF

!sort x-values
CALL Sort( P_x , order_x )

!reverse order_ccw if optional argument Reverse=.TRUE.
!! ReverseOrder = .FALSE. meaning proceed from left to right along x-axis
!! ReverseOrder = .TRUE.  meaning proceed from right to left along x-axis
!we go 
IF( MERGE( ReverseOrder , .FALSE. , PRESENT(ReverseOrder) ) )THEN
 order_x = Reverse( order_x )
 U_base = (/ +c_0 , +c_1 /)
ELSE
 U_base = (/ +c_0 , -c_1 /)
ENDIF

IF( N<=3 )THEN
 order_ccw(1:N) = (/order_x(1:N-1),0/)
 RETURN
ENDIF

!allocate the stack
CALL ALLOCATE( Stack , N )

!itrail = order_x(0) is implicitly given with the starting direction
U_reference = U_base
itrail = 0
icheck = order_x(1)
ilead  = order_x(2)
j = 2
DO 
 !push the next check point on to the stack
 CALL Push( icheck , Stack )

 !get the "test direction" from the "check point" to the "lead point"
 U_test = xyDIRECTION_PP( Cn(:,icheck) , Cn(:,ilead) )

 !calculate the angle from the "reference direction" to the "test direction"
 t = xyANGLE_UU( U_reference , U_test )

 !if the angle is greater than \pi then we have a non-convex portion
 IF( t>c_PI )THEN
  !this is not a point on the counter clockwise chain
  CALL Pop( ijunk , Stack )
    
  !pop the current point as the "check point"
  CALL Pop( icheck , Stack )
   
  !determine the new reference direction
  IF( IsEmpty(Stack) )THEN
   !set the reference direction to the base direction if we are back at the beginning
   U_reference = U_base
  ELSE
   !make the last point on the stack the trailing point
   itrail = Top(Stack)
   !get the new reference
   U_reference = xyDIRECTION_PP( Cn(:,itrail) , Cn(:,icheck) )
   !ilead stays the same
  ENDIF
 
 ELSE
  
  !the checked point is now the trail point
  itrail = icheck

  !set the lead point
  icheck = ilead
  
  !increment and possibly exit
  j = j + 1
  IF( j==N+1 )EXIT

  !get the next "lead point"
  ilead = order_x(j)
   
  !set the new "reference direction" to the old "test direction"
  U_reference = U_test
  
 ENDIF

END DO

!final update
order_ccw(1:Stack%top) = Stack%stack(1:Stack%top)
order_ccw(Stack%top+1:) = 0

CALL DEALLOCATE( Stack )