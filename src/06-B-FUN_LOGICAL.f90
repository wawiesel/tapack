!!### MODULE: FUNCTION  FUN_LOGICAL
MODULE FUN_LOGICAL
!!#### PURPOSE
!! Provide a function to convert from a string to a
!! logical.

!!#### USAGE
!
!    L = REAL(S,KIND)
!
!! where <KIND> is the desired <KIND> of logical <L>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L,KIND_L1,KIND_L2,KIND_L4,KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!#### GLOBAL USER MODULES
USE USR_Lstar0                                                     !!((05-A-USR_Lstar0.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### INTRINSIC OVERLOADING
INTERFACE LOGICAL
 MODULE PROCEDURE LOGICAL_S
END INTERFACE

!!#### ACCESS
PUBLIC :: LOGICAL

CONTAINS


PURE ELEMENTAL FUNCTION LOGICAL_S(S,KIND) RESULT(Lstar0)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S
INTEGER                    ,INTENT(IN) :: KIND

!!#### REQUIRED OUTPUT
TYPE(TYPE_Lstar0) :: Lstar0

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
Lstar0% KIND = KIND

SELECT CASE( Lstar0% KIND )
 CASE( KIND_L1 ) ; READ(S,*,IOSTAT=jerr) Lstar0%L1
 CASE( KIND_L2 ) ; READ(S,*,IOSTAT=jerr) Lstar0%L2
 CASE( KIND_L4 ) ; READ(S,*,IOSTAT=jerr) Lstar0%L4
 CASE DEFAULT   ; jerr = 1
END SELECT

IF( jerr/=0 )THEN
 Lstar0% KIND = -1
END IF

!!--end--
END FUNCTION


END MODULE
