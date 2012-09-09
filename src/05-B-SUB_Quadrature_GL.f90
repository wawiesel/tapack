MODULE SUB_Quadrature_GL
!!#### PURPOSE
!! Provides Gauss-Legendre quadrature set abcissas
!! and weights on an interval, x.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp , KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_PI                 !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_Sequence                                 !!((03-A-FUN_Sequence.f90))
USE SUB_CLEAR                                    !!((04-A-SUB_CLEAR.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE Quadrature_GL
 MODULE PROCEDURE Quadrature_GL_Rsp
 MODULE PROCEDURE Quadrature_GL_Rdp
ENDINTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Quadrature_GL


CONTAINS

PURE SUBROUTINE Quadrature_GL_Rsp( abcissas , weights , domain , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_Rdp),PARAMETER :: EPS=3.0e-14_KIND_Rdp  !set the tolerance a little lower than in the
INCLUDE "05-B-SUB_Quadrature_GL.f90.hdr"    !pure double precision procedure below
!!--begin--
INCLUDE "05-B-SUB_Quadrature_GL.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE Quadrature_GL_Rdp( abcissas , weights , domain , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_Rdp),PARAMETER :: EPS=1.0e-14_KIND_Rdp
INCLUDE "05-B-SUB_Quadrature_GL.f90.hdr"
!!--begin--
INCLUDE "05-B-SUB_Quadrature_GL.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
