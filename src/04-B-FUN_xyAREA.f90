!!# FUNCTION MODULE <<FUN_xyAREA>>
MODULE FUN_xyAREA


!!## PURPOSE
!! Calculate the unsigned AREA of various shapes.


!!## DETAILS
!! The current shapes supported are
!! @ ellipse <El>
!! @ slices <Sli>


!!## DEPENDENCIES
USE FUN_xySAREA                               !!((03-A-FUN_xySAREA.f90))



!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE xyAREA_El
 MODULE PROCEDURE xyAREA_El_Rsp
 MODULE PROCEDURE xyAREA_El_Rdp
END INTERFACE
INTERFACE xyAREA_Sli
 MODULE PROCEDURE xyAREA_Sli_Rsp
 MODULE PROCEDURE xyAREA_Sli_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: xyAREA_El
PUBLIC :: xyAREA_El_Rsp
PUBLIC :: xyAREA_El_Rdp
PUBLIC :: xyAREA_Sli
PUBLIC :: xyAREA_Sli_Rsp
PUBLIC :: xyAREA_Sli_Rdp


!!## MODULE PROCEDURES
CONTAINS



!!### FUNCTION <<xyAREA_El_Rsp>>
FUNCTION xyAREA_El_Rsp( El ) RESULT(AREA)

!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp ,ONLY: c_PI             !!((02-A-PAR_Constants_Rsp.f90))

INCLUDE "04-B-FUN_xyAREA_El.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyAREA_El.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <<xyAREA_El_Rdp>>
FUNCTION xyAREA_El_Rdp( El ) RESULT(AREA)

!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp ,ONLY: c_PI             !!((02-A-PAR_Constants_Rdp.f90))

INCLUDE "04-B-FUN_xyAREA_El.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyAREA_El.f90.bdy"
!!--end--
END FUNCTION





!!### FUNCTION <<xyAREA_Sli_Rsp>>
FUNCTION xyAREA_Sli_Rsp( N , Sli ) RESULT(AREA)

!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rsp ,ONLY: c_PI             !!((02-A-PAR_Constants_Rsp.f90))

INCLUDE "04-B-FUN_xyAREA_Sli.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyAREA_Sli.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <<xyAREA_Sli_Rdp>>
FUNCTION xyAREA_Sli_Rdp( N , Sli ) RESULT(AREA)

!!#### LOCAL MAPPINGS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE PAR_Constants_Rdp ,ONLY: c_PI             !!((02-A-PAR_Constants_Rdp.f90))

INCLUDE "04-B-FUN_xyAREA_Sli.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_xyAREA_Sli.f90.bdy"
!!--end--
END FUNCTION



END MODULE
