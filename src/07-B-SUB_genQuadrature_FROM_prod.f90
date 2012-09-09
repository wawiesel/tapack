!!# SUBROUTINE MODULE <<SUB_genQuadrature_FROM_prod>>
MODULE SUB_genQuadrature_FROM_prod

!!## PURPOSE
!! Return a general quadrature set on the unit
!! sphere from a product quadrature.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!## EXTERNAL PROCEDURES
USE SUB_CLEAR                                  !!((04-A-SUB_CLEAR.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE LISTING
INTERFACE genQuadrature_FROM_prod
 MODULE PROCEDURE genQuadrature_FROM_prod_Rsp
 MODULE PROCEDURE genQuadrature_FROM_prod_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: genQuadrature_FROM_prod


!!## MODULE PROCEDURES
CONTAINS


!!### PURE SUBROUTINE: genQuadrature_FROM_prod_Rsp
PURE SUBROUTINE genQuadrature_FROM_prod_Rsp( xa , xp , wa , wp , &
                                   m_map , x , w , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_genQuadrature_FROM_prod.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_genQuadrature_FROM_prod.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: genQuadrature_FROM_prod_Rdp
PURE SUBROUTINE genQuadrature_FROM_prod_Rdp( xa , xp , wa , wp , &
                                   m_map , x , w , errint , errmsg )
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "07-B-SUB_genQuadrature_FROM_prod.f90.hdr"
!!--begin--
INCLUDE "07-B-SUB_genQuadrature_FROM_prod.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
