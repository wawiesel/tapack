!!# FUNCTION MODULE <<FUN_xyEVAL>>
MODULE FUN_xyEVAL

!!## PURPOSE
!! Evaluate stuff.


!!## EXTERNAL MODULES
USE FUN_Error                                   !!((04-A-FUN_Error.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## PROCEDURE OVERLOADING

INTERFACE xyEVAL_Lin2P
 MODULE PROCEDURE xyEVAL_Lin2P_Rsp
 MODULE PROCEDURE xyEVAL_Lin2P_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: xyEVAL_Lin2P


!!## CONTAINED PROCEDURES
CONTAINS


!!### FUNCTION <xyEVAL_Lin2P_Rsp>
FUNCTION xyEVAL_Lin2P_Rsp( Lin2 , P ) RESULT(f)
!!#### LOCAL MAPPINGS
USE PAR_Constants_Rsp                           !!((02-A-PAR_Constants_Rsp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_xyEVAL_Lin2P.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_xyEVAL_Lin2P.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION <xyEVAL_Lin2P_Rdp>
FUNCTION xyEVAL_Lin2P_Rdp( Lin2 , P ) RESULT(f)
!!#### LOCAL MAPPINGS
USE PAR_Constants_Rdp                           !!((02-A-PAR_Constants_Rdp.f90))
USE KND_IntrinsicTypes,ONLY: KIND_R => KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_xyEVAL_Lin2P.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_xyEVAL_Lin2P.f90.bdy"
!!--end--
END FUNCTION






END MODULE
