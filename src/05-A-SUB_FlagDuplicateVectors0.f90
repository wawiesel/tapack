!!## MODULE SUBROUTINE: FlagDuplicateVectors0
MODULE SUB_FlagDuplicateVectors0
!!### PURPOSE
!! Flag duplicates in a list of vectors from a *SORTED*
!! list of vectors by setting the duplicates to the
!! error number, and optionally returning a mask that
!! is true only for the unique vectors.

!!### USAGE
!
!     CALL FlagDuplicateVectors0( sorted_VECS [,tol] [,reltol] [,unique])
!
!! where <sorted_VECS> is a list of *SORTED* vectors
!! <VECS(1:Nd,:)>, where <Nd> is the number of dimensions
!! and <tol>, <reltol> are the tolerances and relative
!! tolerances of the comparison for duplicate status,
!! respectively.
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
INTERFACE FlagDuplicateVectors0
 MODULE PROCEDURE FlagDuplicateVectors0_Rsp
 MODULE PROCEDURE FlagDuplicateVectors0_Rdp
END INTERFACE


!!### PUBLIC ACCESS LIST
PUBLIC :: FlagDuplicateVectors0


!!## PROCEDURE LISTING
CONTAINS


!!### PURE SUBROUTINE: FlagDuplicateVectors0_Rsp
PURE SUBROUTINE FlagDuplicateVectors0_Rsp(sorted_VECS,tol,reltol,unique)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-SUB_FlagDuplicateVectors0.f90.hdr"
!!--begin--
INCLUDE "05-A-SUB_FlagDuplicateVectors0.f90.bdy"
!!--end--
END SUBROUTINE


!!### PURE SUBROUTINE: FlagDuplicateVectors0_Rdp
PURE SUBROUTINE FlagDuplicateVectors0_Rdp(sorted_VECS,tol,reltol,unique)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-SUB_FlagDuplicateVectors0.f90.hdr"
!!--begin--
INCLUDE "05-A-SUB_FlagDuplicateVectors0.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
