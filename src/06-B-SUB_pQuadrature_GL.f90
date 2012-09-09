!!## SUBROUTINE MODULE: <pQuadrature_GL>
MODULE SUB_pQuadrature_GL
!!### PURPOSE
!! Set up an polar-angle $[0,\Pi]$
!! Quadrature using the Gauss-Legendre quadrature set.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!### EXTERNAL PROCEDURES
USE SUB_Quadrature_GL                          !!((05-B-SUB_Quadrature_GL.f90))
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))
USE FUN_Reverse                                !!((04-A-FUN_Reverse.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### PROCEDURE LISTING
INTERFACE pQuadrature_GL
 MODULE PROCEDURE pQuadrature_GL_Rsp
 MODULE PROCEDURE pQuadrature_GL_Rdp
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: pQuadrature_GL


!!### MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: <pQuadrature_GL_Rsp>
PURE SUBROUTINE pQuadrature_GL_Rsp( pOrder , xp,wp , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_pQuadrature_GL.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_pQuadrature_GL.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: <pQuadrature_GL_Rdp>
PURE SUBROUTINE pQuadrature_GL_Rdp( pOrder , xp,wp , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_pQuadrature_GL.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_pQuadrature_GL.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
