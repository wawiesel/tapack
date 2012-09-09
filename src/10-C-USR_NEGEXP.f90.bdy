!get value
!IF( .NOT.ASSOCIATED(y) )THEN
! val = exp(-s)
!ELSE IF( s>y(SIZE(y)) )THEN
! val = exp(-s)
!ELSE

!!!!bisection search part!!!!!

!LB is lower bound
LB = 1

!UB is upper bound
UB = SIZE(x)

!i is current division (starts in center)
!       <-----LB----i-----UB---->
i = (LB+UB)/2

!start search
DO WHILE( i/=LB ) 
 IF( s<x(i) )THEN
  UB = i
 ELSE
  LB = i
 ENDIF
 i = (LB+UB)/2
END DO


f2  = (s-x(LB))/(x(UB)-x(LB))
f1  = 1._KIND_R - f2
val = f1*y(LB) + f2*y(UB)


