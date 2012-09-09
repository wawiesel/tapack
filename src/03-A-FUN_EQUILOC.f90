!!# FUNCTION MODULE >>EQUILOC<<
MODULE FUN_EQUILOC

!!## PURPOSE
!! Searches for the location in an unsorted list (1D array)
!! where the element is equal to a value, with an optional
!! tolerance.



!!## USAGE
!
!          loc = EQUILOC( list , val [ , tol ] )
!
!!  returns the first location in <list> where <list(loc)==val>.
!!  If the optional tolerance is specified, then <loc> is the
!!  first location in <list> where <ABS(list(loc)-val)<=tol>.



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_Csp,KIND_Cdp,&
                             KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
                             KIND_L1,KIND_L2,KIND_L4,&
                             KIND_S



!!## HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## OVERLOADING
INTERFACE EQUILOC
 MODULE PROCEDURE EQUILOC_Rsp
 MODULE PROCEDURE EQUILOC_Rdp
 MODULE PROCEDURE EQUILOC_I1
 MODULE PROCEDURE EQUILOC_I2
 MODULE PROCEDURE EQUILOC_I4
 MODULE PROCEDURE EQUILOC_I8
 MODULE PROCEDURE EQUILOC_L1
 MODULE PROCEDURE EQUILOC_L2
 MODULE PROCEDURE EQUILOC_L4
END INTERFACE



!!## PUBLIC ACCESS LIST
PUBLIC :: EQUILOC



!!## MODULE PROCEDURES
CONTAINS



PURE FUNCTION EQUILOC_Rsp( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_Rdp( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_Csp( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_C.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC_C.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_Cdp( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_C.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC_C.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_I1( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_I2( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_I4( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_I8( list , val , tol )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_L1( list , val )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_L.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC_L.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_L2( list , val )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_L.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC_L.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION EQUILOC_L4( list , val )  RESULT(loc)
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_EQUILOC_L.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_EQUILOC_L.f90.bdy"
!!--end--
END FUNCTION

END MODULE
