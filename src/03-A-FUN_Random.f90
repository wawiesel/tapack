!!## MODULE: FUNCTION  Random
MODULE FUN_Random
!!### PURPOSE
!! Returns a random number in the interval <l>, left-inclusive.


!!### USAGE
!
!              x = Random( l(1:2) )
!
!! where $x$ satisfies $ l(1) <= x <  l(2)$ for real, complex, and integer $x$.


!!### DETAILS
!! Note this USEs a basic call to the intrinsic random number generator,
!! <CALL RANDOM_NUMBER( rand )>.  Therefore, one should initialize the
!! random number generator seed with <CALL RANDOM_SEED()> before
!! calling <Random>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R,KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_Csp,KIND_Cdp,&
                             KIND_I1,KIND_I2,KIND_I4,KIND_I8

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### FUNCTION OVERLOADING
INTERFACE Random
 MODULE PROCEDURE Random_I1
 MODULE PROCEDURE Random_I2
 MODULE PROCEDURE Random_I4
 MODULE PROCEDURE Random_I8
 MODULE PROCEDURE Random_Rsp
 MODULE PROCEDURE Random_Rdp
 MODULE PROCEDURE Random_Csp
 MODULE PROCEDURE Random_Cdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Random


CONTAINS


FUNCTION Random_I1( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_I.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Random_I2( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_I.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Random_I4( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_I.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Random_I8( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_I.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Random_Rsp( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_R.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Random_Rdp( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_R.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Random_Csp( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_C.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_C.f90.bdy"
!!--end--
END FUNCTION


FUNCTION Random_Cdp( l ) RESULT(x)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Random_C.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Random_C.f90.bdy"
!!--end--
END FUNCTION

ENDMODULE
