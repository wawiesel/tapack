!!# MODULE: SUBROUTINE <SUB_levQuadratureNormalize>
MODULE SUB_levQuadratureNormalize

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_4_times_PI       !!((02-A-PAR_Constants_Rdp.f90))


!!## EXTERNAL PROCEDURES
USE FUN_Reverse                                !!((04-A-FUN_Reverse.f90))
USE FUN_SIZEa                                  !!((06-B-FUN_SIZEa.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE levQuadratureNormalize
 MODULE PROCEDURE levQuadratureNormalize_Rsp
 MODULE PROCEDURE levQuadratureNormalize_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: levQuadratureNormalize


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: levQuadratureNormalize_Rsp
PURE SUBROUTINE levQuadratureNormalize_Rsp( xa,xp,wa,wp,Norm)

!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_levQuadratureNormalize.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_levQuadratureNormalize.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: levQuadratureNormalize_Rdp
PURE SUBROUTINE levQuadratureNormalize_Rdp( xa,xp,wa,wp,Norm)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_levQuadratureNormalize.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_levQuadratureNormalize.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
