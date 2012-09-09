!!# MODULE: FUN_Substitute
MODULE FUN_Substitute
!!## PURPOSE
!! Replace each special symbolic variable <var(s)> with an
!! integer <int(s)> and format <fmt(s)>.



!!## USAGE
!
! B = Substitute( A , var , int [, fmt] )
!
!! where <A> is a <string> or <varying_string> and
!! <B> is a <varying_string>.



!!## EXAMPLES
!
! >>> A = "Test$g"
!
! >>> B = Substitute(A,(/"$g"/),(/1/))
! <<< B = "Test1"
!
! >>> B = Substitute(A,(/"$g"/),(/1234/),FMTS=(/"(i6.6)"/))
! <<< B = "Test001234"
!


!!## AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com | 2006


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Sfile,KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!### EXTERNAL STANDARDS
USE ISO_varying_string                         !!((03-A-ISO_varying_string.f90))

!!### EXTERNAL FUNCTIONS
USE FUN_STR                                    !!((05-B-FUN_STR.f90))
USE FUN_IsError                                !!((05-A-FUN_IsError.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## IDENTIFICATION
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: file_ = "06-C-FUN_Substitute.f90"
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: mod_  = "FUN_Substitute"

!!## DEFAULT ACCESS
PRIVATE

!!## OVERLOADING
INTERFACE Substitute
 MODULE PROCEDURE Substitute_VS
 MODULE PROCEDURE Substitute_S
END INTERFACE


!!## PUBLIC ROUTINES
PUBLIC :: Substitute,Substitute_VS


CONTAINS


FUNCTION Substitute_S(A,var,int,fmt) RESULT(B)

INCLUDE "06-C-FUN_Substitute_S.f90.hdr"
!!--begin--
INCLUDE "06-C-FUN_Substitute.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Substitute_VS(A,var,int,fmt) RESULT(B)

INCLUDE "06-C-FUN_Substitute_VS.f90.hdr"
!!--begin--
INCLUDE "06-C-FUN_Substitute.f90.bdy"
!!--end--
END FUNCTION


END MODULE
