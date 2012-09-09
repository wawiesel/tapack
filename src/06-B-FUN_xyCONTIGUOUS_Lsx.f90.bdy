
!! Handle optional arguments.
IF( PRESENT(tol) )THEN
 tol_ = tol
ELSE
 tol_ = DEFAULT_tol
END IF

IF( PRESENT(Noisy) )THEN
 Noisy_ = Noisy
ELSE
 Noisy_ = DEFAULT_Noisy
END IF

!! Initialize.
order = 0
placed = .FALSE.

!! Start by placing the first face.
order(1)  = 1
placed(1) = .TRUE.
P_base = Lsx(:,2,1)

!! Loop through points.
j = 0
m = 1
num = 0
DO  
 !counter
 num = num + 1

 j = MOD(j,N) + 1
 
 IF( placed(j) )CYCLE
 
 IF( num>SIZE(Lsx,3)**2 )THEN
  order = Error(order(1))
  EXIT
 END IF

 !! Find a match.
 DO l=1,2
  
  !! Get a new point to match.
  P_match = Lsx(:,l,j)
  !IF( Noisy_ )WRITE(*,*)P_match
  
  IF( xyDIST_PP(P_base,P_match)<=tol_ )THEN
   placed(j) = .TRUE.
   EXIT
  END IF
  
 END DO
 
 !get the new base point
 IF( placed(j) )THEN
  !IF( Noisy_ )THEN
  ! WRITE(*,*)"m=",m
  ! WRITE(*,*)"j=",j
  ! WRITE(*,*)"l=",l
  !END IF
  
  m = m + 1
  order(j) = MERGE(m,-m,l==1)
  l = MOD(l,2) + 1
  P_base  = Lsx(:,l,j)
  
  !IF( Noisy_ )WRITE(*,*)"P_base=",P_base
  
  IF( m==N )EXIT
 END IF

END DO