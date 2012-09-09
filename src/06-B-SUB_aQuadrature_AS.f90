!!# SUBROUTINE MODULE: <aQuadrature_AS>
MODULE SUB_aQuadrature_AS

!!## PURPOSE
!! Set up a azimuthal angle $[0,2\Pi]$ Quadrature
!! using the Abu-Shamays quadrature set.


!! This data is taken from Table 1 (azimuthal quadratures) of the paper:
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


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PROCEDURES
USE SUB_aQuadratureReflect                     !!((05-B-SUB_aQuadratureReflect.f90))
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE aQuadrature_AS
 MODULE PROCEDURE aQuadrature_AS_Rsp
 MODULE PROCEDURE aQuadrature_AS_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: aQuadrature_AS


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: <aQuadrature_AS_Rsp>
PURE SUBROUTINE aQuadrature_AS_Rsp( aOrder , xa,wa , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_aQuadrature_AS.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_aQuadrature_AS.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: <aQuadrature_AS_Rdp>
PURE SUBROUTINE aQuadrature_AS_Rdp( aOrder , xa,wa , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_aQuadrature_AS.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_aQuadrature_AS.f90.bdy"
!!--end--
END SUBROUTINE







END MODULE
