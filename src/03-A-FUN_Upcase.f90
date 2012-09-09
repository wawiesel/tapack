!!# FUNCTION MODULE: <FUN_Upcase>
MODULE FUN_Upcase

!!## PURPOSE
!! Convert a string to upper case.


!!## USAGE
!
!    S = Upcase( T )
!
!! where <T> is a string and <S> is a string of the
!! same length with only upper case letters.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))


!!## HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ACCESS LIST
PUBLIC :: Upcase



!!## MODULE PROCEDURES
CONTAINS


!!### PURE ELEMENTAL FUNCTION: <Upcase>
PURE ELEMENTAL FUNCTION Upcase(STRING)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: STRING

!!#### REQUIRED OUTPUT
CHARACTER(LEN=LEN(STRING),KIND=KIND_S)  :: Upcase

!!#### PARAMETERS
INTEGER,PARAMETER :: LO_to_UP  = IACHAR("A")-IACHAR("a")
INTEGER,PARAMETER :: IACHAR_a  = IACHAR("a")
INTEGER,PARAMETER :: IACHAR_z  = IACHAR("z")

!!#### LOCAL VARIABLES
INTEGER :: i,j

!!--begin--
!start with STRING
Upcase = STRING

!uppercase everything that is lowercase
DO i=1,LEN_TRIM(STRING)
 j = IACHAR(STRING(i:i))
 IF( IACHAR_a<=j .AND. j<=IACHAR_z )THEN
  Upcase(i:i) = ACHAR(j+LO_to_UP)
 END IF
END DO

!!--end--
END FUNCTION


END MODULE
