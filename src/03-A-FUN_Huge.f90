!!## MODULE: FUNCTION  Huge
MODULE FUN_HUGE

!!### PURPOSE
!! Define HUGE for the complex type.



!!### METHOD
!! <HUGE(C)> is defined as $C = A + Bi$ with real part, <A = HUGE(A)>
!! and complex part, <B = REAL(1,KIND(B))> to avoid overflow.



!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C,KIND_Csp,KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))



!!### HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!### DEFAULT IMPLICIT
IMPLICIT NONE



!!### DEFAULT ACCESS
PRIVATE



!!### INTRINSIC OVERLOADING
INTERFACE HUGE
 MODULE PROCEDURE HUGE2_Csp
 MODULE PROCEDURE HUGE2_Cdp
END INTERFACE



!!### PARAMETERS DECLARATIONS
COMPLEX(KIND_C)  ,PARAMETER :: HUGE_C   = HUGE(1._KIND_C)
COMPLEX(KIND_Csp),PARAMETER :: HUGE_Csp = HUGE(1._KIND_Csp)
COMPLEX(KIND_Cdp),PARAMETER :: HUGE_Cdp = HUGE(1._KIND_Cdp)



!!### PUBLIC ACCESS LIST
!! @ parameters
PUBLIC :: HUGE_C,HUGE_Csp,HUGE_Cdp
!! @ function
PUBLIC :: HUGE



!!## MODULE PROCEDURES
CONTAINS



!!### PURE FUNCTION: HUGE2_Csp
PURE FUNCTION HUGE2_Csp(C) RESULT(HUGE)
!!#### REQUIRED INPUT
COMPLEX(KIND_Csp),INTENT(IN) :: C
!!#### REQUIRED OUTPUT
COMPLEX(KIND_Csp) :: HUGE
!!--begin--
HUGE = HUGE_Csp
!!--end--
END FUNCTION



!!### PURE FUNCTION: HUGE2_Cdp
PURE FUNCTION HUGE2_Cdp(C) RESULT(HUGE)
!!#### REQUIRED INPUT
COMPLEX(KIND_Cdp),INTENT(IN) :: C
!!#### REQUIRED OUTPUT
COMPLEX(KIND_Cdp) :: HUGE
!!--begin--
HUGE = HUGE_Cdp
!!--end--
END FUNCTION


END MODULE
