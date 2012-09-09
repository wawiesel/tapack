!###BINARY OPERATOR: sEQ
MODULE BOP_sEQ

!!### PURPOSE
!! Defines binary operator <.sEQ.> which checks
!! whether 2 strings are equal DISREGARDING differences
!! in the case of characters in the two strings.



!!### USAGE
!! The operator form is:
!
!    IsEqual = A .sEQ. B
!
!! The funcational form is:
!
!    IsEqual = sEQ(A,B)
!


!!### DETAILS
!! Using .sEQ. is MUCH faster than using using a case
!! changer on both strings to facilitate comparison:
!
!            IsEqual = LoCase(A)==LoCase(B)
!
!! or even just one string if the case of one
!! string is known (B is lower case for example):
!
!            IsEqual = LoCase(A)==B
!



!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S !!((01-A-KND_IntrinsicTypes.f90))



!!### HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!### DEFAULT IMPLICIT
IMPLICIT NONE



!!### DEFAULT ACCESS
PRIVATE



!!### LOCAL PARAMETERS
INTEGER,PARAMETER :: LO_to_UP  = IACHAR("A")-IACHAR("a")
INTEGER,PARAMETER :: IACHAR_a  = IACHAR("a")
INTEGER,PARAMETER :: IACHAR_z  = IACHAR("z")



!!### OPERATOR DEFINITION
INTERFACE OPERATOR(.sEQ.)
 MODULE PROCEDURE sEQ_S
END INTERFACE



!!### INTERFACE DEFINITIONS
INTERFACE sEQ
 MODULE PROCEDURE sEQ_S
END INTERFACE



!!### PUBLIC ACCESS LIST
PUBLIC :: OPERATOR(.sEQ.)
PUBLIC :: sEQ



!!## MODULE PROCEDURES
CONTAINS


!!### PURE ELEMENTAL FUNCTION: sEQ_S
PURE ELEMENTAL FUNCTION sEQ_S(A,B) RESULT(IsEqual)

!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: A,B

!!#### REQUIRED OUTPUT
LOGICAL :: IsEqual

!!#### LOCAL VARIABLES
INTEGER :: iA,iB,j

!!--begin--
IsEqual = A==B
IF( IsEqual )THEN
 RETURN
ELSE
 IF( LEN_TRIM(A)/=LEN_TRIM(B) )RETURN
 DO j=1,LEN_TRIM(A)
  iB = IACHAR(B(j:j))
  IF( IACHAR_a<=iB .AND. iB<=IACHAR_z )iB=iB+LO_to_UP
  iA = IACHAR(A(j:j))
  IF( IACHAR_a<=iA .AND. iA<=IACHAR_z )iA=iA+LO_to_UP
  IsEqual = iA==iB
  IF( .NOT.IsEqual )RETURN
 END DO
END IF

!!--end--
END FUNCTION


END MODULE
