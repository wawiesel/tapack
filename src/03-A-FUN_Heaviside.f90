!!# MODULE: FUNCTION  Heaviside
MODULE FUN_Heaviside

!!## PURPOSE
!! Defines function <Heaviside(x)>, which is $1$ for
!! $x \ge 0$ and $0$ elsewhere.



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))



!!## HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## PROCEDURE OVERLOADING
INTERFACE Heaviside
 MODULE PROCEDURE Heaviside_Rsp
 MODULE PROCEDURE Heaviside_Rdp
END INTERFACE



!!## PUBLIC ACCESS LIST
PUBLIC :: Heaviside



!!## MODULE PROCEDURES
CONTAINS



!!### ELEMENTAL FUNCTION: Heaviside_Rsp
PURE ELEMENTAL FUNCTION Heaviside_Rsp( x ) RESULT(y)
USE PAR_Constants_Rsp ,ONLY: c_1,c_0           !!((02-A-PAR_Constants_Rsp.f90))
!!#### REQUIRED INPUT
REAL(KIND_Rsp),INTENT(IN) :: x
!!#### REQUIRED OUTPUT
REAL(KIND_Rsp)            :: y
!!--begin--
y = MERGE( c_1 , c_0 , x>=c_0 )
!!--end--
END FUNCTION



!!### ELEMENTAL FUNCTION: Heaviside_Rdp
PURE ELEMENTAL FUNCTION Heaviside_Rdp( x ) RESULT(y)
USE PAR_Constants_Rdp ,ONLY: c_1,c_0           !!((02-A-PAR_Constants_Rdp.f90))
!!#### REQUIRED INPUT
REAL(KIND_Rdp),INTENT(IN) :: x
!!#### REQUIRED OUTPUT
REAL(KIND_Rdp)            :: y
!!--begin--
y = MERGE( c_1 , c_0 , x>=c_0 )
!!--end--
END FUNCTION

END MODULE
