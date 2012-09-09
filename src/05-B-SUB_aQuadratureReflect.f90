!!## SUBROUTINE MODULE: <aQuadratureReflect>
MODULE SUB_aQuadratureReflect
!!### PURPOSE
!! Set up a azimuthal angle $[0,2\Pi]$ Quadrature
!! using the Gauss-Legendre quadrature set.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))

!!### EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_0,c_PI_by_2,c_2,c_2_times_PI !!((02-A-PAR_Constants_Rdp.f90))

!!### EXTERNAL PROCEDURES
USE FUN_Reverse                                            !!((04-A-FUN_Reverse.f90))
USE SUB_CLEAR                                              !!((04-A-SUB_CLEAR.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### PROCEDURE LISTING
INTERFACE aQuadratureReflect
 MODULE PROCEDURE aQuadratureReflect_Rsp
 MODULE PROCEDURE aQuadratureReflect_Rdp
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: aQuadratureReflect


!!### MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: <aQuadratureGL_Rsp>
PURE SUBROUTINE aQuadratureReflect_Rsp( aOrder , xa , wa , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-SUB_aQuadratureReflect.f90.hdr"
!!--begin--
INCLUDE "05-B-SUB_aQuadratureReflect.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: <aQuadratureGL_Rdp>
PURE SUBROUTINE aQuadratureReflect_Rdp( aOrder , xa , wa , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-SUB_aQuadratureReflect.f90.hdr"
!!--begin--
INCLUDE "05-B-SUB_aQuadratureReflect.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
