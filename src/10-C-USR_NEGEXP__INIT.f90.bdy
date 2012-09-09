!get local tolerance
tol_ = DEFAULT(EPSILON(1._KIND_R)*10._KIND_R,tol)


!initialize
IF( .NOT.ASSOCIATED(x) )THEN
 N = 10
 ALLOCATE( x(N),y(N) )
 x = ERROR(1._KIND_R)
 x(1)   = 0._KIND_R
 x(2)   = MAX_s
 x(3:4) = x(2)
 y(1)   = 1._KIND_R
 y(2)   = exp(-MAX_s)
 y(3:4) = y(2)
 i       = 2
 LastPos = 4
 CALL RefineTable()

!add new data
ELSE IF( Max_s>x(SIZE(x)) )THEN
 
 i = SIZE(x)
 CALL RefineTable()

END IF

CONTAINS


SUBROUTINE RefineTable()
DO 
 !reallocate if out of bounds
 IF( LastPos+1>N )THEN
  CALL REALLOCATE(x,SIZE(x))
  CALL REALLOCATE(y,SIZE(y))
 END IF
 N = SIZE(x)
 !write(*,*)n
 
 !2.a. get new x
 x(i)   = (x(i-1)+x(i+1))/2._KIND_R
 !2.b. calculate new y
 y(i)   = exp( -x(i) )
 !3.a. calculate linear interpolation of x
 y_     = (y(i+1)+y(i-1))/2._KIND_R
 
 !write(*,*)i
 !write(*,*)x(i-1),x(i),x(i+1)
 !write(*,*)y(i-1),y(i),y(i+1)
 !write(*,*)y(i-1), y_ ,y(i+1)
 !write(*,*)abs(y(i)-y_),tol
 !read(*,*)
 
 
 !check increment
 IF( IsApprox( y(i) , y_ , tol=tol_) )THEN
  
  !move to next increment if linear interpolation was acceptable
  i = i + 1
  IF( x(i)==MAX_s )EXIT

  !1.a. bump down x
  x(LastPos+1) = x(LastPos)
  !1.b. bump down y
  y(LastPos+1) = y(LastPos)
  LastPos = LastPos + 1

 !make increment smaller
 ELSE
  x(i+1) = x(i)
  y(i+1) = y(i)
 END IF
  
END DO

CALL REALLOCATE( x , LastPos-N-1 )
CALL REALLOCATE( y , LastPos-N-1 )

END SUBROUTINE
