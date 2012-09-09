!!#FUNCTION MODULE: JustifyCenter
MODULE FUN_JustifyCenter
!!##PURPOSE
!! Adjust a string to the center of its field.



!!##USAGE
!! There are two ways to call <JustifyCenter>.
!!
!!
!! 1. Explicit fieldwidth <N>
!
!  T = JustifyCenter( N , S )
!
!! where <N> is the fieldwidth to justify <S>, resulting in justified
!! output <T>.
!!
!!
!!
!! 2. Implicit fieldwidth <N>
!
!  T = JustifyCenter( S )
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
INTERFACE JustifyCenter
 MODULE PROCEDURE JustifyCenter1_A0S
 MODULE PROCEDURE JustifyCenter2_A0S
 MODULE PROCEDURE JustifyCenter1_A1S
 MODULE PROCEDURE JustifyCenter2_A1S
END INTERFACE



!!##PUBLIC ACCESS LIST
PUBLIC :: JustifyCenter
PUBLIC :: JustifyCenter1_A0S
PUBLIC :: JustifyCenter2_A0S
PUBLIC :: JustifyCenter1_A1S
PUBLIC :: JustifyCenter2_A1S



!!##PROCEDURE LISTING
CONTAINS



!!###PURE FUNCTION: JustifyCenter1_A0S
PURE FUNCTION JustifyCenter1_A0S( N , S ) RESULT(SOUT)
!!####PURPOSE
!! Explicitly adjust a scalar string to the center of its field.

!!####REQUIRED INPUT
INTEGER,INTENT(IN) :: N
CHARACTER(LEN=N,KIND=KIND_S),INTENT(IN) :: S

!!####REQUIRED OUTPUT
CHARACTER(LEN=N,KIND=KIND_S) :: SOUT

!!####LOCAL VARIABLES
INTEGER :: K,L,M

!!--begin--

!adjust left
SOUT = ADJUSTL(S)

!get the trimmed length
L = LEN_TRIM(SOUT(1:N))

!get number of spaces total
M = N - L

!get number of spaces to put on right
K = M/2

!get the centered string
SOUT = REPEAT(" ",M-K)//SOUT(1:L)//REPEAT(" ",K)

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyCenter2_A0S
PURE FUNCTION JustifyCenter2_A0S( S ) RESULT(SOUT)
!!####PURPOSE
!! Implicitly adjust a scalar string to the center of its field.

!!####REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S

!!####REQUIRED OUTPUT
CHARACTER(LEN=LEN(S),KIND=KIND_S) :: SOUT

!!####LOCAL VARIABLES
INTEGER :: K,L,M,N

!!--begin--

SOUT = JustifyCenter1_A0S( LEN(S) , S )

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyCenter1_A1S
PURE FUNCTION JustifyCenter1_A1S( N , S ) RESULT(SOUT)
!!####PURPOSE
!! Explicitly adjust an array of strings to the center of its field.

!!####REQUIRED INPUT
INTEGER,INTENT(IN) :: N
CHARACTER(LEN=N,KIND=KIND_S),INTENT(IN) :: S(:)

!!####REQUIRED OUTPUT
CHARACTER(LEN=N,KIND=KIND_S) :: SOUT(SIZE(S))

!!####LOCAL VARIABLES
INTEGER :: J

!!--begin--

DO J=1,SIZE(S)
 SOUT(J) = JustifyCenter1_A0S( N , S(J) )
END DO

!!--end--
END FUNCTION



!!###PURE FUNCTION: JustifyCenter2_A1S
PURE FUNCTION JustifyCenter2_A1S( S ) RESULT(SOUT)
!!####PURPOSE
!! Implicitly adjust an array of strings to the center of its field.

!!####REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S(:)

!!####REQUIRED OUTPUT
CHARACTER(LEN=LEN(S),KIND=KIND_S) :: SOUT(SIZE(S))

!!####LOCAL VARIABLES
INTEGER :: J

!!--begin--

DO J=1,SIZE(S)
 SOUT(J) = JustifyCenter1_A0S( LEN(S(J)) , S(J) )
END DO

!!--end--
END FUNCTION



END MODULE
