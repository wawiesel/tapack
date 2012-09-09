!!# SUBROUTINE MODULE: <SUB_genQuadrature>
MODULE SUB_genQuadrature
!!## PURPOSE
!! Return a general quadrature set on the unit
!! sphere.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rdp,ONLY: c_2                !!((02-A-PAR_Constants_Rdp.f90))


!!## EXTERNAL PROCEDURES
USE SUB_Reallocate                             !!((04-B-SUB_Reallocate.f90))
USE SUB_levQuadrature_LMLQ                     !!((08-B-SUB_levQuadrature_LMLQ.f90))
USE SUB_Sort_quick ,ONLY: Sort =>Sort_quick    !!((03-A-SUB_Sort_quick.f90))
USE SUB_Sort2_quick,ONLY: Sort2=>Sort2_quick   !!((06-A-SUB_Sort2_quick.f90))
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))
USE FUN_Error                                  !!((04-A-FUN_Error.f90))
!USE SUB_genQuadrature_AbuShamays

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE genQuadrature
 MODULE PROCEDURE genQuadrature_Rsp
 MODULE PROCEDURE genQuadrature_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: genQuadrature


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: <genQuadrature_Rsp>
PURE SUBROUTINE genQuadrature_Rsp( Quadrature , Order , &
                                   x , w , &
                                   errint , errmsg , &
                                   UniformZGeometry )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-B-SUB_genQuadrature.f90.hdr"
!!--begin--
INCLUDE "09-B-SUB_genQuadrature.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: <genQuadrature_Rdp>
PURE SUBROUTINE genQuadrature_Rdp( Quadrature , Order , &
                                   x , w , &
                                   errint , errmsg , &
                                   UniformZGeometry )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "09-B-SUB_genQuadrature.f90.hdr"
!!--begin--
INCLUDE "09-B-SUB_genQuadrature.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
