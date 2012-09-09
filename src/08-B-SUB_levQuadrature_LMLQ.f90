!!# SUBROUTINE MODULE: <SUB_levQuadrature_LMLQ>
MODULE SUB_levQuadrature_LMLQ

!!## PURPOSE
!! Generate the Level Symmetric (LQ) quadrature sets in
!! Lewis and Miller's "Computational Methods of Neutron
!! Transport", 1993, American Nuclear Society.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: &                !!((01-A-KND_IntrinsicTypes.f90))
  KIND_Rsp,KIND_Rdp                           !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_2               !!((02-A-PAR_Constants_Rdp.f90))


!!## EXTERNAL PROCEDURES
USE SUB_levQuadratureReflect                  !!((07-B-SUB_levQuadratureReflect.f90))
USE SUB_levQuadratureNormalize                !!((07-B-SUB_levQuadratureNormalize.f90))
USE FUN_NEARLOC                               !!((06-A-FUN_NEARLOC.f90))
USE FUN_Error                                 !!((04-A-FUN_Error.f90))
USE FUN_IsError                               !!((05-A-FUN_IsError.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE levQuadrature_LMLQ
 MODULE PROCEDURE levQuadrature_LMLQ_Rsp
 MODULE PROCEDURE levQuadrature_LMLQ_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: levQuadrature_LMLQ


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: <levQuadrature_LMLQ_Rsp>
PURE SUBROUTINE levQuadrature_LMLQ_Rsp( N,xa,xp,wa,wp,Nm )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-SUB_levQuadrature_LMLQ.f90.hdr"
!!--begin--
INCLUDE "08-B-SUB_levQuadrature_LMLQ.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: <levQuadrature_LMLQ_Rdp>
PURE SUBROUTINE levQuadrature_LMLQ_Rdp( N,xa,xp,wa,wp,Nm )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-SUB_levQuadrature_LMLQ.f90.hdr"
!!--begin--
INCLUDE "08-B-SUB_levQuadrature_LMLQ.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
