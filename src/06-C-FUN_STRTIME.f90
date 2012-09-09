!!# FUNCTION MODULE: <STRTIME>
MODULE FUN_STRTIME

!!## PURPOSE
!! Returns a runtime in a human-readable, compact format.


!!## USAGE
!
! WRITE(*,*)STRTIME(dt)
!
!! Note: <STRTIME(dt)> is 7 characters---always.


!!## ACCESS AND IMPLICIT
IMPLICIT NONE
PRIVATE


!!## DETAILS
!! The passed time <dt> (or $\Delta t$) is handled
!! in the following way:
!!
!! @ if $\Delta t < t_{us} $ or $\Delta t >t_{max}$ then use
!!   exponential format with seconds, otherwise
!!
!! @ if $\Delta t >= t_{s}$, then use seconds, otherwise
!!
!! @ if $\Delta t >= t_{ms}$, then use milliseconds, otherwise
!!
!! @ if $\Delta t >= t_{us}$, then use microseconds.
!!


!!## LOCAL VARIABLES
REAL :: tmax=60.
REAL :: ts=1.0
REAL :: tms=1.e-3
REAL :: tus=1.e-6


!!## PUBLIC ACCESS LIST
PUBLIC :: TEST_STRTIME,STRTIME


!!## PROCEDURES
CONTAINS


!!### FUNCTION <STRTIME>
FUNCTION STRTIME(dt)

!!#### REQUIRED INPUT
REAL,INTENT(IN) :: dt

!!#### REQUIRED OUTPUT
CHARACTER(7) :: STRTIME

!!#### LOCAL VARIABLES
CHARACTER(10) :: s

!!--begin--

!use seconds in exponential format
IF( dt>tmax .OR. dt<tus )THEN

 WRITE(s,"(Es10.2)")ABS(dt)
 s = ADJUSTL(s)

 !need both exponent digits
 IF( ABS(dt)<1.e-9 .OR. ABS(dt)>1.e+9 )THEN
  STRTIME = s(1:2)//"E"//s(6:6)//s(7:8)//"s"
 !just need one
 ELSE
  STRTIME = s(1:3)//"E"//s(6:6)//s(8:8)//"s"
 END IF
!use seconds
ELSE IF( dt>=ts )THEN

 WRITE(s,"(f10.4)")ABS(dt)
 s = ADJUSTL(s)
 STRTIME = s(1:6)//"s"

!use millseconds
ELSE IF( dt>=tms )THEN

 WRITE(s,"(f9.3)")ABS(dt*1.E3)
 s = ADJUSTL(s)
 STRTIME = s(1:5)//"ms"

!use micrseconds
ELSE IF( dt>=tus )THEN

 WRITE(s,"(f9.3)")ABS(dt*1.E6)
 s = ADJUSTL(s)
 STRTIME = s(1:5)//"us"

END IF

!!--end--
END FUNCTION

SUBROUTINE TEST_STRTIME()
REAL :: r
INTEGER :: i,j

!!--begin--
WRITE(*,"(a12,2x,a12)")"r (seconds)","STRTIME(r)"
DO i=1,10
 CALL RANDOM_NUMBER(r)
 r=r/1.E7
 DO j=1,10
  r = r*10.
 WRITE(*,"(e12.5,2x,a)")r,STRTIME(r)
END DO

END DO

!!--end--
END SUBROUTINE


END MODULE
