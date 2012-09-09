!!## MODULE: FUNCTION  IntervalBisection
MODULE FUN_IntervalBisection

!!### PURPOSE
!! use the standard bisection search to determine the interval
!! indices <l(1:2)> in the ordered list of <N> real or
!! integer values, <x> which contain <x0>.



!!### USAGE
!
!       l = IntervalBisection( x0 , x [, MAX_span] )
!
!! where the returned interval <l> has three possibilities
!
!    1. l(1)=LB,l(2)=UB  for x(LB) <= x0 < x(UB)
!    2. l(1)=0 ,l(2)=1   for          x0 < x(1)
!    3. l(1)=N ,l(2)=N+1 for x(N)  <= x0
!
!! and the optional argument <MAX_span> indicates the desired
!! maximum number of points to include in the interval,
!! <UB-LB+1 = MAX_span>.



!!### METHOD
!! Standard bisection search.



!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
                             KIND_S



!!### HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!#### DEFAULT IMPLICIT
IMPLICIT NONE



!!#### DEFAULT ACCESS
PRIVATE



!!### INTERFACE DEFINITION
INTERFACE IntervalBisection
 MODULE PROCEDURE IntervalBisection_Rsp
 MODULE PROCEDURE IntervalBisection_Rdp
 MODULE PROCEDURE IntervalBisection_I1
 MODULE PROCEDURE IntervalBisection_I2
 MODULE PROCEDURE IntervalBisection_I4
 MODULE PROCEDURE IntervalBisection_I8
 MODULE PROCEDURE IntervalBisection_S
END INTERFACE



!!### PUBLIC ACCESS LIST
PUBLIC :: IntervalBisection



!!## MODULE PROCEDURES
CONTAINS



PURE FUNCTION IntervalBisection_Rsp( x0 , x , MAX_span ) RESULT(l)
INTEGER,PARAMETER :: KIND_x=KIND_Rsp
INCLUDE "03-A-FUN_IntervalBisection_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IntervalBisection.f90.sup"
INCLUDE "03-A-FUN_IntervalBisection.f90.alg"
INCLUDE "03-A-FUN_IntervalBisection.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION IntervalBisection_Rdp( x0 , x , MAX_span ) RESULT(l)
INTEGER,PARAMETER :: KIND_x=KIND_Rdp
INCLUDE "03-A-FUN_IntervalBisection_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IntervalBisection.f90.sup"
INCLUDE "03-A-FUN_IntervalBisection.f90.alg"
INCLUDE "03-A-FUN_IntervalBisection.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION IntervalBisection_I1( x0 , x , MAX_span ) RESULT(l)
INTEGER,PARAMETER :: KIND_x=KIND_I1
INCLUDE "03-A-FUN_IntervalBisection_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IntervalBisection.f90.sup"
INCLUDE "03-A-FUN_IntervalBisection.f90.alg"
INCLUDE "03-A-FUN_IntervalBisection.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION IntervalBisection_I2( x0 , x , MAX_span ) RESULT(l)
INTEGER,PARAMETER :: KIND_x=KIND_I2
INCLUDE "03-A-FUN_IntervalBisection_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IntervalBisection.f90.sup"
INCLUDE "03-A-FUN_IntervalBisection.f90.alg"
INCLUDE "03-A-FUN_IntervalBisection.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION IntervalBisection_I4( x0 , x , MAX_span ) RESULT(l)
INTEGER,PARAMETER :: KIND_x=KIND_I4
INCLUDE "03-A-FUN_IntervalBisection_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IntervalBisection.f90.sup"
INCLUDE "03-A-FUN_IntervalBisection.f90.alg"
INCLUDE "03-A-FUN_IntervalBisection.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION IntervalBisection_I8( x0 , x , MAX_span ) RESULT(l)
INTEGER,PARAMETER :: KIND_x=KIND_I8
INCLUDE "03-A-FUN_IntervalBisection_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IntervalBisection.f90.sup"
INCLUDE "03-A-FUN_IntervalBisection.f90.alg"
INCLUDE "03-A-FUN_IntervalBisection.f90.bdy"
!!--end--
END FUNCTION



PURE FUNCTION IntervalBisection_S( x0 , x , MAX_span ) RESULT(l)
INTEGER,PARAMETER :: KIND_x=KIND_S
INCLUDE "03-A-FUN_IntervalBisection_S.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_IntervalBisection.f90.sup"
INCLUDE "03-A-FUN_IntervalBisection.f90.alg"
INCLUDE "03-A-FUN_IntervalBisection.f90.bdy"
!!--end--
END FUNCTION

END MODULE
