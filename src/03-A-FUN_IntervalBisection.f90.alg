!LB is lower bound
LB = 1

!UB is upper bound
UB = N

!i is current division (starts in center)
!       <-----LB----i-----UB---->
i = (LB+UB)/2

!start search
DO WHILE( i/=LB ) 
 IF( x0<x(i) )THEN
  UB = i
 ELSE
  LB = i
 ENDIF
 i = (LB+UB)/2
END DO