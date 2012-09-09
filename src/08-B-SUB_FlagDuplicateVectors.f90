!!## MODULE SUBROUTINE: FlagDuplicateVectors
MODULE SUB_FlagDuplicateVectors
!!### PURPOSE
!! Flag duplicates in a list of vectors by setting
!! the duplicates to the error number, and optionally
!! returning a mask that is true only for the unique vectors.

!!### USAGE
!
!     CALL FlagDuplicateVectors( VECS [,tol] [,reltol] [,unique])
!
!! where <VECS> is a list of vectors <VECS(1:Nd,:)>, where <Nd>
!! is the number of dimensions and <tol>, <reltol> are the
!! tolerances and relative tolerances of the comparison
!! for duplicate status, respectively.
!!
!! The vectors that are duplicates will be changed to the
!! error number, and have a value of <.FALSE.> in the <unique> mask.
!!
!! If you provide a mask, then getting a list
!! of unique vectors is easy.
!
!     Nd = SIZE(VECS,1)
!     Nv = COUNT(unique)
!     ALLOCATE( unique_VECS(Nd,Nv) )
!
!     unique_VECS = PACKvectors( VECS , unique )
!
!! If you don't provide a <unique> mask then pick one dimension to get
!! the <IsError> mask and do it this way:
!
!    DO i=1,Nd
!     unique_VECS(i,:) = PACK( VECS(i,:) , IsError(VECS(1,:)) )
!    END DO
!


!!### DETAILS
!! Optional arguments, <tol> and <reltol> provide
!! absolute and relative tolerances, with <tol> overriding
!! <reltol> if (for some reason) both are present, for determining
!! whether or not two points are actually duplicates.


!!### NOTES
!! You may not mix kinds.


!!### MODULAR KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp     !!((01-A-KND_IntrinsicTypes.f90))


!!### MODULAR PROCEDURES
USE FUN_IsApprox                                   !!((03-A-FUN_IsApprox.f90))
USE FUN_Error                                      !!((04-A-FUN_Error.f90))
USE SUB_FlagDuplicateVectors0                      !!((05-A-SUB_FlagDuplicateVectors0.f90))
USE SUB_Sort2_quick,ONLY: SortVectors=>Sort2_quick !!((06-A-SUB_Sort2_quick.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### OPERATOR DEFINITION
INTERFACE FlagDuplicateVectors
 MODULE PROCEDURE FlagDuplicateVectors_Rsp
 MODULE PROCEDURE FlagDuplicateVectors_Rdp
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: FlagDuplicateVectors


CONTAINS


SUBROUTINE FlagDuplicateVectors_Rsp(VECS,tol,reltol,unique)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-SUB_FlagDuplicateVectors.f90.hdr"
!!--begin--
INCLUDE "08-B-SUB_FlagDuplicateVectors.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE FlagDuplicateVectors_Rdp(VECS,tol,reltol,unique)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp      !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-SUB_FlagDuplicateVectors.f90.hdr"
!!--begin--
INCLUDE "08-B-SUB_FlagDuplicateVectors.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
