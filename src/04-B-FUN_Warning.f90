!!# FUNCTION MODULE: <FUN_Warning>
MODULE FUN_Warning

!!## PURPOSE
!! Provide a warning function, taking an argument and
!! returning the ``warning number''---a number that indicates
!! a variable's value is currently in some unknown warning
!! state.


!!## USAGE
!
!  x = Warning(x)
!


!!## DETAILS
!! This is useful in low-level procedures where passing
!! information about the successful completion of the routine
!! via an information flag is not desired.  Instead, if
!! a warning occurs in the low-level routine, you just set
!! < x=Warning(x) >.  At the appropriate time, a check
!! should be made on the value of <x> to check for warnings
!! using either < x==Warning(x) > or < IsWarning(x) >.


!!## OWNER
! W.A. Wieselquist | william.wieselquist@gmail.com


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes                        !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL STANDARDS
USE ISO_varying_string                        !!((03-A-ISO_varying_string.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Huge                                  !!((03-A-FUN_Huge.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## FUNCTION OVERLOADING
INTERFACE Warning
 MODULE PROCEDURE Warning2_Csp
 MODULE PROCEDURE Warning2_Cdp
 MODULE PROCEDURE Warning2_Rsp
 MODULE PROCEDURE Warning2_Rdp
 MODULE PROCEDURE Warning2_I1
 MODULE PROCEDURE Warning2_I2
 MODULE PROCEDURE Warning2_I4
 MODULE PROCEDURE Warning2_I8
 MODULE PROCEDURE Warning2_S
 MODULE PROCEDURE Warning2_VS
END INTERFACE


!!## LOCAL PARAMETERS
!! @ warning number for intrinsics
INTEGER(KIND_I)             ,PARAMETER :: Warning_I   = HUGE(1_KIND_I)
INTEGER(KIND_I1)            ,PARAMETER :: Warning_I1  = HUGE(1_KIND_I1)
INTEGER(KIND_I2)            ,PARAMETER :: Warning_I2  = HUGE(1_KIND_I2)
INTEGER(KIND_I4)            ,PARAMETER :: Warning_I4  = HUGE(1_KIND_I4)
INTEGER(KIND_I8)            ,PARAMETER :: Warning_I8  = HUGE(1_KIND_I8)
REAL(KIND_R)                ,PARAMETER :: Warning_R   = HUGE(1_KIND_R)
REAL(KIND_Rsp)              ,PARAMETER :: Warning_Rsp = HUGE(1_KIND_Rsp)
REAL(KIND_Rdp)              ,PARAMETER :: Warning_Rdp = HUGE(1_KIND_Rdp)
COMPLEX(KIND_C)             ,PARAMETER :: Warning_C   = HUGE_C
COMPLEX(KIND_Csp)           ,PARAMETER :: Warning_Csp = HUGE_Csp
COMPLEX(KIND_Cdp)           ,PARAMETER :: Warning_Cdp = HUGE_Cdp
CHARACTER(LEN=*,KIND=KIND_S),PARAMETER :: Warning_S   = " "

!!## PUBLIC ACCESS
PUBLIC :: Warning
PUBLIC :: Warning_I,Warning_I1,Warning_I2,Warning_I4,Warning_I8,&
          Warning_R,Warning_Rsp,Warning_Rdp,&
          Warning_C,Warning_Csp,Warning_Cdp,Warning_S


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: <Warning2_Rsp>
PURE FUNCTION Warning2_Rsp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_R.f90.hdr"
!!--begin--
A = Warning_Rsp
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_Rdp>
PURE FUNCTION Warning2_Rdp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_R.f90.hdr"
!!--begin--
A = Warning_Rdp
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_Csp>
PURE FUNCTION Warning2_Csp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_C.f90.hdr"
!!--begin--
A = Warning_Csp
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_Cdp>
PURE FUNCTION Warning2_Cdp(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_C.f90.hdr"
!!--begin--
A = Warning_Cdp
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_I1>
PURE FUNCTION Warning2_I1(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_I.f90.hdr"
!!--begin--
A = Warning_I1
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_I2>
PURE FUNCTION Warning2_I2(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_I.f90.hdr"
!!--begin--
A = Warning_I2
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_I4>
PURE FUNCTION Warning2_I4(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_I.f90.hdr"
!!--begin--
A = Warning_I4
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_I8>
PURE FUNCTION Warning2_I8(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_I.f90.hdr"
!!--begin--
A = Warning_I8
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_S>
PURE FUNCTION Warning2_S(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_S.f90.hdr"
!!--begin--
A = Warning_S
!!--end--
END FUNCTION

!!### PURE FUNCTION: <Warning2_VS>
PURE FUNCTION Warning2_VS(B) RESULT(A)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_Warning_VS.f90.hdr"
!!--begin--
A = Warning_S
!!--end--
END FUNCTION


END MODULE
