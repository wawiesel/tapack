!!# FUNCTION MODULE: <Counter>
MODULE FUN_Counter

!!## PURPOSE
!! Counts from 1 to infinity with successive calls.

!!## USAGE
!
!   I = Counter()
!
!! increments internal counts and returns next one.
!
!
!   I = Counter( .TRUE. )
!
!! returns 0 and initializes the counter.
!
!
!   I = Counter( .FALSE. )
!
!! returns current count.


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## VARIABLES
INTEGER :: COUNTS = 0


!!## ACCESS
PUBLIC :: Counter

!!## MODULE PROCEDURES
CONTAINS

FUNCTION Counter( Init )
LOGICAL,INTENT(IN),OPTIONAL :: Init
INTEGER :: Counter
!!--begin--
IF( PRESENT(Init) )THEN
 IF( Init )THEN
  COUNTS = 0
 END IF
ELSE
 COUNTS = COUNTS + 1
END IF
Counter = COUNTS
!!--end--
END FUNCTION

END MODULE
