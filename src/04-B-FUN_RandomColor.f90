MODULE FUN_RandomColor
!!#### PURPOSE
!! Get a random color in RGBA real format, R,G,B,A in [0,1].

!!#### EXTERNAL PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PROCEDURES
USE SUB_Randomize                              !!((03-A-SUB_Randomize.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE RandomColor
 MODULE PROCEDURE RandomColor_Rsp
 MODULE PROCEDURE RandomColor_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: RandomColor

CONTAINS

FUNCTION RandomColor_Rsp( A ) RESULT(RGBA)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A
REAL(KIND_R) :: RGBA(1:4)
!!--begin--
CALL Randomize( RGBA(1:3) )
RGBA(1) = A
!!--end--
END FUNCTION

FUNCTION RandomColor_Rdp( A ) RESULT(RGBA)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),INTENT(IN) :: A
REAL(KIND_R) :: RGBA(1:4)
!!--begin--
CALL Randomize( RGBA(1:3) )
RGBA(1) = A
!!--end--
END FUNCTION

END MODULE
