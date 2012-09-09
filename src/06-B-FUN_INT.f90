!!# FUNCTION MODULE: <FUN_INT>
MODULE FUN_INT

!!## PURPOSE
!! Overload the integer conversion intrinsic, <INT>,
!! to accept string arguments.


!!## USAGE
!
!    I = INT(S,KIND)
!
!! where <KIND> is the desired <KIND> of integer <I>.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: &                  !!((01-A-KND_IntrinsicTypes.f90))
  KIND_I,KIND_I1,KIND_I2,KIND_I4,KIND_I8,KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!## GLOBAL USER MODULES
USE USR_Istar0                                  !!((05-A-USR_Istar0.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Error                                   !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## INTRINSIC OVERLOADING
INTERFACE INT
 MODULE PROCEDURE INT_S
END INTERFACE

!!## PUBLIC ACCESS
PUBLIC :: INT
PUBLIC :: ASSIGNMENT(=)

!!## CONTAINED PROCEDURES
CONTAINS


PURE ELEMENTAL FUNCTION INT_S(S,KIND) RESULT(Istar0)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S
INTEGER                     ,INTENT(IN) :: KIND

!!#### REQUIRED OUTPUT
TYPE(TYPE_Istar0) :: Istar0

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
Istar0% KIND = KIND

SELECT CASE( Istar0% KIND )
 CASE( KIND_I1 ) ; READ(S,*,IOSTAT=jerr) Istar0%I1
 CASE( KIND_I2 ) ; READ(S,*,IOSTAT=jerr) Istar0%I2
 CASE( KIND_I4 ) ; READ(S,*,IOSTAT=jerr) Istar0%I4
 CASE( KIND_I8 ) ; READ(S,*,IOSTAT=jerr) Istar0%I8
 CASE DEFAULT   ; jerr = 1
END SELECT

IF( jerr/=0 )THEN
 Istar0% KIND = -1
END IF

!!--end--
END FUNCTION


END MODULE
