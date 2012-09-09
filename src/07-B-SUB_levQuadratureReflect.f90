!!# MODULE: SUBROUTINE <SUB_levQuadratureReflect>
MODULE SUB_levQuadratureReflect

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PARAMETERS


!!## EXTERNAL PROCEDURES
USE FUN_Reverse                                !!((04-A-FUN_Reverse.f90))
USE FUN_SIZEa                                  !!((06-B-FUN_SIZEa.f90))
USE SUB_aQuadratureReflect                     !!((05-B-SUB_aQuadratureReflect.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE levQuadratureReflect
 MODULE PROCEDURE levQuadratureReflect_Rsp
 MODULE PROCEDURE levQuadratureReflect_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: levQuadratureReflect


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: levQuadratureReflect_Rsp
PURE SUBROUTINE levQuadratureReflect_Rsp( xa,xp,wa,wp )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_levQuadratureReflect.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_levQuadratureReflect.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: levQuadratureReflect_Rdp
PURE SUBROUTINE levQuadratureReflect_Rdp( xa,xp,wa,wp )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_levQuadratureReflect.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_levQuadratureReflect.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
