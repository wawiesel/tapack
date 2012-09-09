MODULE SUB_SimpSurf
USE KND_IntrinsicTypes   !!((01-A-KND_IntrinsicTypes.f90))
USE FUN_Default          !!((04-A-FUN_Default.f90))

CONTAINS

SUBROUTINE SimpSurf( Unit , f , xs , ys , MakeTracks , XFirst )
INTEGER,INTENT(IN) :: Unit
INTERFACE
 FUNCTION f(x,y)
  USE KND_IntrinsicTypes !!((01-A-KND_IntrinsicTypes.f90))
  REAL(KIND_Rdp),INTENT(IN) :: x,y
  REAL(KIND_Rdp) :: f
 END FUNCTION
END INTERFACE
REAL(KIND_Rdp) :: xs(3),ys(3)
REAL(KIND_Rdp) :: x,y
LOGICAL,INTENT(IN),OPTIONAL :: MakeTracks
LOGICAL,INTENT(IN),OPTIONAL :: XFirst
LOGICAL :: XFirst_

!!--begin--

!do xtracks first
XFirst_ = DEFAULT( .FALSE. , XFirst )

!make outer loop over x
IF( .NOT. XFirst_ )THEN

y = ys(1)
DO

 x = xs(1)

 DO
  WRITE(Unit,*)x,y,f(x,y)
  x = x + xs(2)
  IF( x>xs(3) )EXIT
 END DO

 IF( PRESENT(MakeTracks) )THEN
  IF( MakeTracks )THEN
   WRITE(Unit,*)
  END IF
 END IF
 y = y + ys(2)
 IF( y>ys(3) )EXIT

END DO

ELSE

!make outer loop over x
x = xs(1)
DO

 y = ys(1)

 DO
  WRITE(Unit,*)x,y,f(x,y)
  y = y + ys(2)
  IF( y>ys(3) )EXIT
 END DO

 IF( PRESENT(MakeTracks) )THEN
  IF( MakeTracks )THEN
   WRITE(Unit,*)
  END IF
 END IF
 x = x + xs(2)
 IF( x>xs(3) )EXIT

END DO

END IF


!!--end--
END SUBROUTINE

END MODULE
