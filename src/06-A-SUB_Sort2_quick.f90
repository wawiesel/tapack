!!# MODULE SUBROUTINE: <<Sort2_quick>>
MODULE SUB_Sort2_quick

!!## PURPOSE
!! Sort a list of coordinates dimension-wise.  First we
!! sort the coordinates by x-value, the y-value, then z-value, etc.



!!## USAGE
!!
!     CALL Sort2_quick( VECS [,order] [,tol] [,reltol] )
!!
!! where <VECS> is a list of coordinates <VECS(1:N,:)>, where
!! <N> is the number of dimensions and <order>, is the optional
!! list of the original indices.


!!## DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE SUB_Sort_quick,Sort=>Sort_quick            !!((03-A-SUB_Sort_quick.f90))
USE FUN_Reorder                                !!((05-A-FUN_Reorder.f90))
USE FUN_COUNT_Consecutive                      !!((04-B-FUN_COUNT_Consecutive.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## OPERATOR DEFINITION
INTERFACE Sort2_quick
 MODULE PROCEDURE Sort2_quick_Rsp
 MODULE PROCEDURE Sort2_quick_Rdp
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: Sort2_quick


!!## CONTAINED PROCEDURES
CONTAINS


!!### RECURSIVE PURE SUBROUTINE: <<Sort2_quick_Rsp>>
RECURSIVE PURE SUBROUTINE Sort2_quick_Rsp(VECS,order,tol,reltol)

!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-SUB_Sort2_quick.f90.hdr"
!!--begin--
INCLUDE "06-A-SUB_Sort2_quick.f90.bdy"
!!--end--
END SUBROUTINE



!!### RECURSIVE PURE SUBROUTINE: <<Sort2_quick_Rdp>>
RECURSIVE PURE SUBROUTINE Sort2_quick_Rdp(VECS,order,tol,reltol)

!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-A-SUB_Sort2_quick.f90.hdr"
!!--begin--
INCLUDE "06-A-SUB_Sort2_quick.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
