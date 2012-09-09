!!## MODULE SUBROUTINE: FlagDuplicateScalars0
MODULE SUB_FlagDuplicateScalars0
!!### PURPOSE
!! Flag duplicates in a list of scalars from a *SORTED*
!! list of Scalars by setting the duplicates to the
!! error number, and optionally returning a mask that
!! is true only for the unique Scalars.

!!### USAGE
!
!     CALL FlagDuplicateScalars0( sorted_SCAL [,tol] [,reltol] [,unique])
!
!! where <sorted_SCAL> is a list of *SORTED* Scalars
!! <VECS(1:Nd,:)>, where <Nd> is the number of dimensions
!! and <tol>, <reltol> are the tolerances and relative
!! tolerances of the comparison for duplicate status,
!! respectively.
!!
!! The Scalars that are duplicates will be changed to the
!! error number, and have a value of <.FALSE.> in the <unique> mask.
!!



!!### DETAILS
!! Optional arguments, <tol> and <reltol> provide
!! absolute and relative tolerances, with <tol> overriding
!! <reltol> if (for some reason) both are present, for determining
!! whether or not two points are actually duplicates.



!!### MODULAR KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!### MODULAR PROCEDURES
USE FUN_IsApprox                               !!((03-A-FUN_IsApprox.f90))
USE FUN_Error                                  !!((04-A-FUN_Error.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### OPERATOR DEFINITION
INTERFACE FlagDuplicateScalars0
 MODULE PROCEDURE FlagDuplicateScalars0_Rsp
 MODULE PROCEDURE FlagDuplicateScalars0_Rdp
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: FlagDuplicateScalars0


!!## PROCEDURE LISTING
CONTAINS


!!### PURE SUBROUTINE: FlagDuplicateScalars0_Rsp
PURE SUBROUTINE FlagDuplicateScalars0_Rsp(sorted_SCAL,tol,reltol,unique)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-SUB_FlagDuplicateScalars0.f90.hdr"
!!--begin--
INCLUDE "05-A-SUB_FlagDuplicateScalars0.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: FlagDuplicateScalars0_Rdp
PURE SUBROUTINE FlagDuplicateScalars0_Rdp(sorted_SCAL,tol,reltol,unique)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-SUB_FlagDuplicateScalars0.f90.hdr"
!!--begin--
INCLUDE "05-A-SUB_FlagDuplicateScalars0.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
