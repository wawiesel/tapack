!!# FUNCTION MODULE: <FUN_CMPLX>
MODULE FUN_CMPLX

!!## PURPOSE
!! Overload the complex conversion intrinsic, <CMPLX>,
!! to accept string arguments.

!!## USAGE
!
!    C = CMPLX(S,KIND)
!
!! where <KIND> is the desired <KIND> of complex <C>.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C,KIND_Csp,KIND_Cdp,KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!## GLOBAL USER MODULES
USE USR_Cstar0                                               !!((05-A-USR_Cstar0.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Error                                                !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## INTRINSIC OVERLOADING
INTERFACE CMPLX
 MODULE PROCEDURE CMPLX_S
ENDINTERFACE

!!## ACCESS
PUBLIC :: CMPLX

!!## CONTAINED PROCEDURES
CONTAINS


PURE ELEMENTAL FUNCTION CMPLX_S(S,KIND) RESULT(Cstar0)
!!#### REQUIRED INPUT
CHARACTER(LEN=*,KIND=KIND_S),INTENT(IN) :: S
INTEGER                    ,INTENT(IN) :: KIND

!!#### REQUIRED OUTPUT
TYPE(TYPE_Cstar0) :: Cstar0

!!#### LOCAL VARIABLES
INTEGER :: jerr

!!--begin--
Cstar0% KIND = KIND

SELECT CASE( Cstar0% KIND )
 CASE( KIND_Csp ) ; READ(S,*,IOSTAT=jerr) Cstar0%Csp
 CASE( KIND_Cdp ) ; READ(S,*,IOSTAT=jerr) Cstar0%Cdp
 CASE DEFAULT    ; jerr = 1
END SELECT

IF( jerr/=0 )THEN
 Cstar0% KIND = 0
END IF

!!--end--
END FUNCTION


ENDMODULE
