!!#FUNCTION MODULE: JustifyRight
MODULE FUN_JustifyRight
!!##PURPOSE
!! Adjust a string to the right of its field.



!!##USAGE
!! There are two ways to call <JustifyRight>.
!!
!!
!! 1. Explicit fieldwidth <N>
!
!  T = JustifyRight( N , S )
!
!! where <N> is the fieldwidth to justify <S>, resulting in justified
!! output <T>.
!!
!!
!!
!! 2. Implicit fieldwidth <N>
!
!  T = JustifyRight( S )
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
INTERFACE JustifyRight
 MODULE PROCEDURE JustifyRight1_A0S
 MODULE PROCEDURE JustifyRight2_A0S
 MODULE PROCEDURE JustifyRight1_A1S
 MODULE PROCEDURE JustifyRight2_A1S
END INTERFACE



!!##PUBLIC ACCESS LIST
PUBLIC :: JustifyRight
PUBLIC :: JustifyRight1_A0S
PUBLIC :: JustifyRight2_A0S
PUBLIC :: JustifyRight1_A1S
PUBLIC :: JustifyRight2_A1S



!!##PROCEDURE LISTING
CONTAINS



!!###PURE FUNCTION: JustifyRight1_A0S
PURE FUNCTION JustifyRight1_A0S( N , S ) RESULT(SOUT)
!!####PURPOSE
!! Explicitly adjust a scalar string to the right of its field.

!!####REQUIRED INPUT
INTEGER,INTENT(IN) :: N
CHARACTER(LEN=N,KIND=KIND_S),INTENT(IN) :: S

!!####REQUIRED OUTPUT
CHARACTER(LEN=N,KIND=KIND_S) :: SOUT

!!--begin--

!adjust left
SOUT = ADJUSTL(S)

!adjust first N characters to the right
SOUT = ADJUSTR(SOUT(1:N))

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyRight2_A0S
PURE FUNCTION JustifyRight2_A0S( S ) RESULT(SOUT)
!!####PURPOSE
!! Implicitly adjust a scalar string to the right of its field.

!!####REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S

!!####REQUIRED OUTPUT
CHARACTER(LEN=LEN(S),KIND=KIND_S) :: SOUT

!!--begin--

SOUT = JustifyRight1_A0S( LEN(S) , S )

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyRight1_A1S
PURE FUNCTION JustifyRight1_A1S( N , S ) RESULT(SOUT)
!!####PURPOSE
!! Explicitly adjust an array of strings to the right of its field.

!!####REQUIRED INPUT
INTEGER,INTENT(IN) :: N
CHARACTER(LEN=N,KIND=KIND_S),INTENT(IN) :: S(:)

!!####REQUIRED OUTPUT
CHARACTER(LEN=N,KIND=KIND_S) :: SOUT(SIZE(S))

!!####LOCAL VARIABLES
INTEGER :: J

!!--begin--

DO J=1,SIZE(S)
 SOUT(J) = JustifyRight1_A0S( N , S(J) )
END DO

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyRight2_A1S
PURE FUNCTION JustifyRight2_A1S( S ) RESULT(SOUT)
!!####PURPOSE
!! Implicitly adjust an array of strings to the right of its field.

!!####REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S(:)

!!####REQUIRED OUTPUT
CHARACTER(LEN=LEN(S),KIND=KIND_S) :: SOUT(SIZE(S))

!!####LOCAL VARIABLES
INTEGER :: J

!!--begin--

DO J=1,SIZE(S)
 SOUT(J) = JustifyRight1_A0S( LEN(S(J)) , S(J) )
END DO

!!--end--
END FUNCTION



END MODULE
