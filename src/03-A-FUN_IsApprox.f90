!!## MODULE: FUNCTION  IsApprox
MODULE FUN_IsApprox

!!### PURPOSE
!! Compare two floats in an approximate sense.



!!### USAGE
!
!     TorF = IsApprox( A , B [,tol] [,reltol] )
!
!! where <TorF> is the <.TRUE.> or <.FALSE.> return value, and
!! <A> and <B> are two reals.



!!### DETAILS
!! Optional arguments, <tol> and <reltol> provide
!! absolute and relative tolerances, with <tol> overriding
!! <reltol> if (for some reason) both are present.
!!
!! The actual check is if
!
!                  ABS(A-B)<=tol
!
!! If <tol> is not present then we calculate it via the formula
!
!                  avg=(A+B)/2
!                  tol=reltol*avg
!
!! If <reltol> is not present then we use
!
!                  reltol=SQRT(EPSILON(avg))
!



!!#### NOTES
!! You may not mix kinds.



!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))



!!### HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!#### DEFAULT IMPLICIT
IMPLICIT NONE



!!#### DEFAULT ACCESS
PRIVATE



!!### INTERFACES
INTERFACE IsApprox
 MODULE PROCEDURE IsApprox_A0_Rsp
 MODULE PROCEDURE IsApprox_A0_Rdp
 MODULE PROCEDURE IsApprox_A1_Rsp
 MODULE PROCEDURE IsApprox_A1_Rdp
 MODULE PROCEDURE IsApprox_A2_Rsp
 MODULE PROCEDURE IsApprox_A2_Rdp
END INTERFACE



!!#### PUBLIC ACCESS LIST
PUBLIC :: IsApprox



!!## MODULE PROCEDURES
CONTAINS


PURE FUNCTION IsApprox_A0_Rsp(A,B,tol,reltol) RESULT(IsApprox)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_IsApprox_A0.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IsApprox_A0.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION IsApprox_A0_Rdp(A,B,tol,reltol) RESULT(IsApprox)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_IsApprox_A0.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IsApprox_A0.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION IsApprox_A1_Rsp(A,B,tol,reltol) RESULT(IsApprox)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_IsApprox_A1.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IsApprox_A1.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION IsApprox_A1_Rdp(A,B,tol,reltol) RESULT(IsApprox)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_IsApprox_A1.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IsApprox_A1.f90.bdy"
!!--end--
END FUNCTION



PURE FUNCTION IsApprox_A2_Rsp(A,B,tol,reltol) RESULT(IsApprox)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_IsApprox_A2.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IsApprox_A2.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION IsApprox_A2_Rdp(A,B,tol,reltol) RESULT(IsApprox)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_IsApprox_A2.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IsApprox_A2.f90.bdy"
!!--end--
END FUNCTION

END MODULE
