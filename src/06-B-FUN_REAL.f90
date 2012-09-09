!!### MODULE: FUNCTION  FUN_REAL
MODULE FUN_REAL
!!#### PURPOSE
!! Overload the complex conversion intrinsic, <REAL>,
!! to accept string arguments.

!!#### USAGE
!
!    R = REAL(S,KIND)
!
!! where <KIND> is the desired <KIND> of real <R>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!#### GLOBAL USER MODULES
USE USR_Rstar0                                        !!((05-A-USR_Rstar0.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PUBLIC

!!#### INTRINSIC OVERLOADING
INTERFACE REAL
 MODULE PROCEDURE REAL_S
END INTERFACE

!!#### ACCESS
!PUBLIC :: REAL

CONTAINS

PURE ELEMENTAL FUNCTION REAL_S(S,KIND) RESULT(Rstar0)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S
INTEGER                     ,INTENT(IN) :: KIND

!!#### REQUIRED OUTPUT
TYPE(TYPE_Rstar0) :: Rstar0

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
Rstar0% KIND = KIND

SELECT CASE( Rstar0% KIND )
 CASE( KIND_Rsp ) ; READ(S,*,IOSTAT=jerr) Rstar0%Rsp
 CASE( KIND_Rdp ) ; READ(S,*,IOSTAT=jerr) Rstar0%Rdp
 CASE DEFAULT    ; jerr = 1
END SELECT

IF( jerr/=0 )THEN
 Rstar0% KIND = 0
END IF

!!--end--
END FUNCTION


END MODULE
