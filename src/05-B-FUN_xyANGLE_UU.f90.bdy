!old
!t = ACOS( xyDOT_VV( U_a , U_b ) )
!
!IF( U_a(1)*U_b(2) - U_a(2)*U_b(1)<0._KIND_R )THEN
! t = -t + c_2_times_PI
!END IF 
!

!new
c = ( U_a(1)*U_b(1) + U_a(2)*U_b(2) )

c = max ( c, -1._KIND_R )
c = min ( c, +1._KIND_R )

t = ACOS( c )

IF( U_a(1)*U_b(2) - U_a(2)*U_b(1)<0._KIND_R )THEN
 t = -t + c_2_times_PI
END IF 

