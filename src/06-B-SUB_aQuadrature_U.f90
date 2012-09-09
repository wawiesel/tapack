!!# SUBROUTINE MODULE <<aQuadrature_U>>
MODULE SUB_aQuadrature_U
!!## PURPOSE
!! Set up a azimuthal angle $[0,2\Pi]$ Quadrature
!! using Uniform distribution.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: &                !!((01-A-KND_IntrinsicTypes.f90))
  KIND_Rsp,KIND_Rdp

!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: &                 !!((02-A-PAR_Constants_Rdp.f90))
  c_0,c_PI_by_2,c_2,c_2_times_PI

!!## EXTERNAL PROCEDURES
USE FUN_Reverse                               !!((04-A-FUN_Reverse.f90))
USE SUB_CLEAR                                 !!((04-A-SUB_CLEAR.f90))
USE SUB_aQuadratureReflect                    !!((05-B-SUB_aQuadratureReflect.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE aQuadrature_U
 MODULE PROCEDURE aQuadrature_U_Rsp
 MODULE PROCEDURE aQuadrature_U_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: aQuadrature_U


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: <<aQuadrature_U_Rsp>>
PURE SUBROUTINE aQuadrature_U_Rsp( aOrder , xa,wa , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_aQuadrature_U.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_aQuadrature_U.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: <<aQuadrature_U_Rdp>>
PURE SUBROUTINE aQuadrature_U_Rdp( aOrder , xa,wa , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_aQuadrature_U.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_aQuadrature_U.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
