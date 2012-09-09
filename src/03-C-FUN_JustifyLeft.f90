!!#FUNCTION MODULE: JustifyLeft
MODULE FUN_JustifyLeft
!!##PURPOSE
!! Adjust a string to the left of its field.



!!##USAGE
!! There are two ways to call <JustifyLeft>.
!!
!!
!! 1. Explicit fieldwidth <N>
!
!  T = JustifyLeft( N , S )
!
!! where <N> is the fieldwidth to justify <S>, resulting in justified
!! output <T>.
!!
!!
!!
!! 2. Implicit fieldwidth <N>
!
!  T = JustifyLeft( S )
!
!! where <N> is assumed <N=LEN(S)> to justify <S>, resulting in justified
!! output <T>.
!!
!!



!!##GLOBAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))



!!##DEFAULT IMPLICIT
IMPLICIT NONE



!!##DEFAULT ACCESS
PRIVATE



!!##PROCEDURE OVERLOADING
INTERFACE JustifyLeft
 MODULE PROCEDURE JustifyLeft1_A0S
 MODULE PROCEDURE JustifyLeft2_A0S
 MODULE PROCEDURE JustifyLeft1_A1S
 MODULE PROCEDURE JustifyLeft2_A1S
END INTERFACE



!!##PUBLIC ACCESS LIST
PUBLIC :: JustifyLeft
PUBLIC :: JustifyLeft1_A0S
PUBLIC :: JustifyLeft2_A0S
PUBLIC :: JustifyLeft1_A1S
PUBLIC :: JustifyLeft2_A1S



!!##PROCEDURE LISTING
CONTAINS



!!###PURE FUNCTION: JustifyLeft1_A0S
PURE FUNCTION JustifyLeft1_A0S( N , S ) RESULT(SOUT)
!!####PURPOSE
!! Explicitly adjust a scalar string to the left of its field.

!!####REQUIRED INPUT
INTEGER,INTENT(IN) :: N
CHARACTER(LEN=N,KIND=KIND_S),INTENT(IN) :: S

!!####REQUIRED OUTPUT
CHARACTER(LEN=N,KIND=KIND_S) :: SOUT

!!--begin--

!adjust left and be done with it
SOUT = ADJUSTL(S)

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyLeft2_A0S
PURE FUNCTION JustifyLeft2_A0S( S ) RESULT(SOUT)
!!####PURPOSE
!! Implicitly adjust a scalar string to the left of its field.

!!####REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S

!!####REQUIRED OUTPUT
CHARACTER(LEN=LEN(S),KIND=KIND_S) :: SOUT

!!--begin--

SOUT = JustifyLeft1_A0S( LEN(S) , S )

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyLeft1_A1S
PURE FUNCTION JustifyLeft1_A1S( N , S ) RESULT(SOUT)
!!####PURPOSE
!! Explicitly adjust an array of strings to the left of its field.

!!####REQUIRED INPUT
INTEGER,INTENT(IN) :: N
CHARACTER(LEN=N,KIND=KIND_S),INTENT(IN) :: S(:)

!!####REQUIRED OUTPUT
CHARACTER(LEN=N,KIND=KIND_S) :: SOUT(SIZE(S))

!!####LOCAL VARIABLES
INTEGER :: J

!!--begin--

DO J=1,SIZE(S)
 SOUT(J) = JustifyLeft1_A0S( N , S(J) )
END DO

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyLeft2_A1S
PURE FUNCTION JustifyLeft2_A1S( S ) RESULT(SOUT)
!!####PURPOSE
!! Implicitly adjust an array of strings to the left of its field.

!!####REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S(:)

!!####REQUIRED OUTPUT
CHARACTER(LEN=LEN(S),KIND=KIND_S) :: SOUT(SIZE(S))

!!####LOCAL VARIABLES
INTEGER :: J

!!--begin--

DO J=1,SIZE(S)
 SOUT(J) = JustifyLeft1_A0S( LEN(S(J)) , S(J) )
END DO

!!--end--
END FUNCTION



END MODULE
