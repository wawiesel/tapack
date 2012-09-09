!!## MODULE: SUBROUTINE FindUniqueVectors
MODULE SUB_FindUniqueVectors
!!### PURPOSE
!! Find unique vectors in <r> and
!! return the number of unique vectors, along
!! with a reordering array, <order> for sorting
!! the vectors in order and leaving out duplicates.


!!### USAGE
!! First get the number of unique vectors and the
!! ordering.
!
!     CALL FindUniqueVectors( r , NUM_Unique , order [,r_unique] )
!
!! Then allocate a new vector.
!
!     Nd = SIZE(r,1)
!     ALLOCATE( r2(Nd,NUM_Unique) )
!
!! Finally, use the <Reorder> function with right-side
!! reordering to get the ordered array and <PACK> the
!! output into <r2>.
!
!     r2 = PACK( ReorderVectors(r,order,SIDE="R") , order>0 )
!
!! Inclusion of the optional pointer <r_unique> is the easiest
!! way to get a sorted list of unique vectors, because
!! that's what it is!



!!### DETAILS
!! Optional arguments, <tol> and <reltol> provide
!! absolute and relative tolerances, with <tol> overriding
!! <reltol> if (for some reason) both are present, for determining
!! whether or not two points are actually duplicates.



!!### MODULAR KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp     !!((01-A-KND_IntrinsicTypes.f90))


!!### MODULAR PROCEDURES
USE FUN_IsApprox                                   !!((03-A-FUN_IsApprox.f90))
USE FUN_Error                                      !!((04-A-FUN_Error.f90))
USE FUN_PACK2,ONLY: PackVectors=>PACK2             !!((03-A-FUN_PACK2.f90))
USE PRN_ColumnVectors                              !!((12-C-PRN_ColumnVectors.f90))
USE SUB_Sort2_quick,ONLY: SortVectors=>SORT2_quick !!((06-A-SUB_Sort2_quick.f90))
USE SUB_FlagDuplicateVectors0                      !!((05-A-SUB_FlagDuplicateVectors0.f90))
USE SUB_Reallocate                                 !!((04-B-SUB_Reallocate.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### OPERATOR DEFINITION
INTERFACE FindUniqueVectors
 MODULE PROCEDURE FindUniqueVectors_Rsp
 MODULE PROCEDURE FindUniqueVectors_Rdp
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: FindUniqueVectors


!!## PROCEDURE LISTING
CONTAINS


!!### SUBROUTINE: FindUniqueVectors_Rsp
SUBROUTINE FindUniqueVectors_Rsp(r,NUM_Unique,order,r_unique,&
  tol,reltol,Unit,Interactive)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-SUB_FindUniqueVectors.f90.hdr"
!!--begin--
INCLUDE "13-B-SUB_FindUniqueVectors.f90.bdy"
!!--end--
END SUBROUTINE


!!### SUBROUTINE: FindUniqueVectors_Rdp
SUBROUTINE FindUniqueVectors_Rdp(r,NUM_Unique,order,r_unique,&
  tol,reltol,Unit,Interactive)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "13-B-SUB_FindUniqueVectors.f90.hdr"
!!--begin--
INCLUDE "13-B-SUB_FindUniqueVectors.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
