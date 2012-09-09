!!## MODULE FUNCTION: Integrate1S_trapezoid
MODULE FUN_Integrate1S_trapezoid
!!### PURPOSE
!! Uses the trapezoid rule to integrate a function <f>
!! defined by a set values <f> and the corresponding
!! set of set of x-values <x>.
!!

!!### USAGE
!! The integration may be performed with a call
!
!        integral = Integrate1S_trapezoid( f , x )
!
!! where the <f> is the list of <N> function values,
!! $f_i=f(x_i),i=1,...,N$ and <x> are those x-values,
!! $x_i,x=1,...,N$.

!!### METHOD
!! The trapezoid rule for integration,
!!
!!  $$ integral = \sum_{i=1}^{N-1} \bar{f}_i \Delta x_i, $$
!!
!! where $\Delta x_i= x_{i+1}-x_i$ and
!! $\bar{f}_i = 0.5(f_i+f_{i+1}) $.
!!

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))


!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE Integrate1S_trapezoid
 MODULE PROCEDURE Integrate1S_trapezoid_yRsp_xRsp
 MODULE PROCEDURE Integrate1S_trapezoid_yRsp_xRdp
 MODULE PROCEDURE Integrate1S_trapezoid_yRdp_xRsp
 MODULE PROCEDURE Integrate1S_trapezoid_yRdp_xRdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Integrate1S_trapezoid

CONTAINS

FUNCTION Integrate1S_trapezoid_yRsp_xRsp( f , x , &
  fbarin , delxin , fbarout , delxout) RESULT(integral)
!!#### LOCAL KINDS
INTEGER,PARAMETER :: KIND_x=KIND_Rsp , KIND_f=KIND_Rsp
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.hdr" !function header
!!--begin--
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.sup" !setup
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.bdy" !function body
!!--end--
END FUNCTION


FUNCTION Integrate1S_trapezoid_yRdp_xRsp( f , x , &
  fbarin , delxin , fbarout , delxout) RESULT(integral)
!!#### LOCAL KINDS
INTEGER,PARAMETER :: KIND_x=KIND_Rsp , KIND_f=KIND_Rdp
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.hdr"  !function header
!!--begin--
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.sup"  !setup
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.bdy" !algorithm
!!--end--
END FUNCTION


FUNCTION Integrate1S_trapezoid_yRsp_xRdp( f , x , &
  fbarin , delxin , fbarout , delxout) RESULT(integral)
!!#### LOCAL KINDS
INTEGER,PARAMETER :: KIND_x=KIND_Rdp , KIND_f=KIND_Rsp
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.hdr"  !function header
!!--begin--
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.sup"  !setup
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.bdy" !algorithm
!!--end--
END FUNCTION


FUNCTION Integrate1S_trapezoid_yRdp_xRdp( f , x , &
  fbarin , delxin , fbarout , delxout) RESULT(integral)
!!#### LOCAL KINDS
INTEGER,PARAMETER :: KIND_x=KIND_Rdp , KIND_f=KIND_Rdp
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.hdr"  !function header
!!--begin--
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.sup"  !setup
INCLUDE "04-B-FUN_Integrate1S_trapezoid.f90.bdy" !algorithm
!!--end--
END FUNCTION


END MODULE
