!!# MODULE: SUBROUTINE <SUB_prodQuadrature>
MODULE SUB_prodQuadrature
!!## PURPOSE
!! Return a product quadrature set on the unit
!! sphere (each polar angle (from z-axis) has the same set of
!! azimuthal angles (angles in xy-plane).


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_2                !!((02-A-PAR_Constants_Rdp.f90))


!!## EXTERNAL PROCEDURES
USE SUB_aQuadrature_GL                         !!((06-B-SUB_aQuadrature_GL.f90))
USE SUB_aQuadrature_AS                         !!((06-B-SUB_aQuadrature_AS.f90))
USE SUB_aQuadrature_U                          !!((06-B-SUB_aQuadrature_U.f90))

USE SUB_pQuadrature_dGL                        !!((06-B-SUB_pQuadrature_dGL.f90))
USE SUB_pQuadrature_GL                         !!((06-B-SUB_pQuadrature_GL.f90))
USE SUB_pQuadrature_AS                         !!((06-B-SUB_pQuadrature_AS.f90))

USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))
USE FUN_Error                                  !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE prodQuadrature
 MODULE PROCEDURE prodQuadrature_Rsp
 MODULE PROCEDURE prodQuadrature_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: prodQuadrature


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: prodQuadrature_Rsp
PURE SUBROUTINE prodQuadrature_Rsp( aQuadrature , aOrder , &
                                    pQuadrature , pOrder , &
                                    xa , xp , wa , wp , &
                                    errint , errmsg , &
                                    UniformZGeometry )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_prodQuadrature.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_prodQuadrature.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: prodQuadrature_Rdp
PURE SUBROUTINE prodQuadrature_Rdp( aQuadrature , aOrder , &
                                    pQuadrature , pOrder , &
                                    xa , xp , wa , wp , &
                                    errint , errmsg , &
                                    UniformZGeometry )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_prodQuadrature.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_prodQuadrature.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
