!!# FUNCTION MODULE: <FUN_Error>
MODULE FUN_Error

!!## PURPOSE
!! Provide a error function, taking an argument and
!! returning the ``error number''---a number that indicates
!! a variable's value is currently in some unknown error
!! state.


!!## USAGE
!
!  x = Error(x)
!


!!## DETAILS
!! This is useful in low-level procedures where passing
!! information about the successful completion of the routine
!! via an information flag is not desired.  Instead, if
!! a error occurs in the low-level routine, you just set
!! < x=Error(x) >.  At the appropriate time, a check
!! should be made on the value of <x> to check for errors
!! using either < x==Error(x) > or < IsError(x) >.


!!## OWNER
! W.A. Wieselquist | william.wieselquist@gmail.com


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes                        !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL STANDARDS
USE ISO_varying_string                        !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Huge                                  !!((03-A-FUN_Huge.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE Error
 MODULE PROCEDURE Error_E0Csp
 MODULE PROCEDURE Error_E0Cdp
 MODULE PROCEDURE Error_E0Rsp
 MODULE PROCEDURE Error_E0Rdp
 MODULE PROCEDURE Error_E0I1
 MODULE PROCEDURE Error_E0I2
 MODULE PROCEDURE Error_E0I4
 MODULE PROCEDURE Error_E0I8
 MODULE PROCEDURE Error_E0S
 MODULE PROCEDURE Error_E0VS
END INTERFACE


!!### EXTERNAL PARAMETERS
INTEGER(KIND_I)             ,PARAMETER :: Error_I   = -HUGE(1_KIND_I)
INTEGER(KIND_I1)            ,PARAMETER :: Error_I1  = -HUGE(1_KIND_I1)
INTEGER(KIND_I2)            ,PARAMETER :: Error_I2  = -HUGE(1_KIND_I2)
INTEGER(KIND_I4)            ,PARAMETER :: Error_I4  = -HUGE(1_KIND_I4)
INTEGER(KIND_I8)            ,PARAMETER :: Error_I8  = -HUGE(1_KIND_I8)
REAL(KIND_R)                ,PARAMETER :: Error_R   = -HUGE(1._KIND_R)
REAL(KIND_Rsp)              ,PARAMETER :: Error_Rsp = -HUGE(1._KIND_Rsp)
REAL(KIND_Rdp)              ,PARAMETER :: Error_Rdp = -HUGE(1._KIND_Rdp)
COMPLEX(KIND_C)             ,PARAMETER :: Error_C   = -HUGE_C
COMPLEX(KIND_Csp)           ,PARAMETER :: Error_Csp = -HUGE_Csp
COMPLEX(KIND_Cdp)           ,PARAMETER :: Error_Cdp = -HUGE_Cdp
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: Error_S   = " "


!!### PUBLIC ACCESS LIST
!! @ function
PUBLIC :: Error
!! @ constants
PUBLIC :: Error_I,Error_I1,Error_I2,Error_I4,Error_I8,&
          Error_R,Error_Rsp,Error_Rdp,&
          Error_C,Error_Csp,Error_Cdp,Error_S


!!## MODULE PROCEDURES
CONTAINS


PURE ELEMENTAL FUNCTION Error_E0Rsp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0R.f90.hdr"
!
A = Error_Rsp
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0Rdp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0R.f90.hdr"
!
A = Error_Rdp
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0Csp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0C.f90.hdr"
!
A = Error_Csp
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0Cdp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0C.f90.hdr"
!
A = Error_Cdp
!
END FUNCTION


PURE ELEMENTAL FUNCTION Error_E0I1(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0I.f90.hdr"
!
A = Error_I1
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0I2(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0I.f90.hdr"
!
A = Error_I2
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0I4(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0I.f90.hdr"
!
A = Error_I4
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0I8(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Error_E0I.f90.hdr"
!
A = Error_I8
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0S(B) RESULT(A)
INCLUDE "04-A-FUN_Error_E0S.f90.hdr"
!
A = Error_S
!
END FUNCTION

PURE ELEMENTAL FUNCTION Error_E0VS(B) RESULT(A)
INCLUDE "04-A-FUN_Error_E0VS.f90.hdr"
!
A = Error_S
!
END FUNCTION



END MODULE
