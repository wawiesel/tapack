!!# SUBROUTINE MODULE: <SUB_genQuadrature_FROM_lev>
MODULE SUB_genQuadrature_FROM_lev
!!## PURPOSE
!! Return a general quadrature set on the unit
!! sphere from a "level symmetric" (lev) quadrature
!! where there is rotational invariance with respect to
!! 90 degree rotations.


!!## EXTERNAL PROCEDURES
USE FUN_SIZEa                                 !!((06-B-FUN_SIZEa.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE genQuadrature_FROM_lev
 MODULE PROCEDURE genQuadrature_FROM_lev_Rsp
 MODULE PROCEDURE genQuadrature_FROM_lev_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: genQuadrature_FROM_lev


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: genQuadrature_FROM_lev_Rsp
PURE SUBROUTINE genQuadrature_FROM_lev_Rsp( xa , xp , wa , wp , &
                                   m_map , x , w , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_genQuadrature_FROM_lev.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_genQuadrature_FROM_lev.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: genQuadrature_FROM_lev_Rdp
PURE SUBROUTINE genQuadrature_FROM_lev_Rdp( xa , xp , wa , wp , &
                                   m_map , x , w , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_genQuadrature_FROM_lev.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_genQuadrature_FROM_lev.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
