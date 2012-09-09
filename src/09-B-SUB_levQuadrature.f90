!!# MODULE: SUBROUTINE <<SUB_levQuadrature>>
MODULE SUB_levQuadrature

!!## PURPOSE
!! Return a product quadrature set on the unit
!! sphere (each polar angle (from z-axis) has the same set of
!! azimuthal angles (angles in xy-plane).


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_2                !!((02-A-PAR_Constants_Rdp.f90))


!!## EXTERNAL PROCEDURES
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))
USE FUN_Error                                  !!((04-A-FUN_Error.f90))
USE SUB_levQuadrature_LMLQ                     !!((08-B-SUB_levQuadrature_LMLQ.f90))
USE FUN_IsError                                !!((05-A-FUN_IsError.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE levQuadrature
 MODULE PROCEDURE levQuadrature_Rsp
 MODULE PROCEDURE levQuadrature_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: levQuadrature


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: levQuadrature_Rsp
PURE SUBROUTINE levQuadrature_Rsp( Quadrature , aOrder , pOrder , &
                                   xa , xp , wa , wp , &
                                   errint , errmsg , &
                                   UniformZGeometry )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-B-SUB_levQuadrature.f90.hdr"
!!--begin--
INCLUDE "09-B-SUB_levQuadrature.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: levQuadrature_Rdp
PURE SUBROUTINE levQuadrature_Rdp( Quadrature , aOrder , pOrder , &
                                   xa , xp , wa , wp , &
                                   errint , errmsg , &
                                                                   UniformZGeometry )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-B-SUB_levQuadrature.f90.hdr"
!!--begin--
INCLUDE "09-B-SUB_levQuadrature.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
