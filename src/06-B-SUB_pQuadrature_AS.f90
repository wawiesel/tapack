!!# SUBROUTINE MODULE: <pQuadrature_AS>
MODULE SUB_pQuadrature_AS

!!## PURPOSE
!! Set up an polar-angle $[0,\Pi]$
!! Quadrature using an Abu-Shamays.


!! This data is taken from Table 2 (polar quadratures) of the paper:
!
! TRANSPORT THEORY AND STATISTICAL PHYSICS, 30(2&3), 169– 204 (2001)
!
!        ANGULAR QUADRATURES
!       FOR IMPROVED TRANSPORT
!           COMPUTATIONS
!
!         I. K. Abu-Shumays
!
!   Bettis Atomic Power Laboratory,
!   RT-Mathematics, ZAP 34DD/RT, P. O. Box 79,
!   West Mifflin, PA 15122-0079
!

!! The <abcissas> are $cos(\theta)$ for $\theta \in [0,\pi]$
!! with corresponding <weights>.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PROCEDURES
USE SUB_Quadrature_GL                          !!((05-B-SUB_Quadrature_GL.f90))
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))
USE FUN_Reverse                                !!((04-A-FUN_Reverse.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE pQuadrature_AS
 MODULE PROCEDURE pQuadrature_AS_Rsp
 MODULE PROCEDURE pQuadrature_AS_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: pQuadrature_AS


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: <pQuadrature_AS_Rsp>
PURE SUBROUTINE pQuadrature_AS_Rsp( pOrder , xp,wp , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_pQuadrature_AS.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_pQuadrature_AS.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: <pQuadrature_AS_Rdp>
PURE SUBROUTINE pQuadrature_AS_Rdp( pOrder , xp,wp , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_pQuadrature_AS.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_pQuadrature_AS.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
