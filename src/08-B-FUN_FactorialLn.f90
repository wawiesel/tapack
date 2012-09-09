!!# MODULE <<FUN_FactorialLn>>
MODULE FUN_FactorialLn
!!## PURPOSE
!! Defines function FactorialLn(k) which returns the real natural log of
!! the factorial of integer k, in double precision.

!!## MODULES
USE FUN_Sequence                                !!((03-A-FUN_Sequence.f90))
USE SUB_Reallocate                              !!((04-B-SUB_Reallocate.f90))
USE FUN_FactorialInt                            !!((05-B-FUN_FactorialInt.f90))
USE FUN_GammaLn                                 !!((05-B-FUN_GammaLn.f90))

!!## PARAMETERS
!! * kind of the real function return value [KIND_R]
!! * kinds of the allowed integer arguments
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_I1,KIND_I2,KIND_I4,KIND_I8

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE !! PUBLIC

!!## FUNCTION OVERLOADING
INTERFACE FactorialLn
 MODULE PROCEDURE FactorialLn_A1I1
 MODULE PROCEDURE FactorialLn_A1I2
 MODULE PROCEDURE FactorialLn_A1I4
 MODULE PROCEDURE FactorialLn_A1I8
 MODULE PROCEDURE FactorialLn_A2I1
 MODULE PROCEDURE FactorialLn_A2I2
 MODULE PROCEDURE FactorialLn_A2I4
 MODULE PROCEDURE FactorialLn_A2I8
ENDINTERFACE
PUBLIC :: FactorialLn


!!## GLOBAL VARIABLES
!! * a list of saved values of the natural log of the factorial [a]
REAL(KIND_R),POINTER :: a(:) => NULL()

CONTAINS

FUNCTION FactorialLn_A2I1( x , Initialize ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A2.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A2.f90.bdy"
!!--end--
ENDFUNCTION

FUNCTION FactorialLn_A2I2( x , Initialize ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A2.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A2.f90.bdy"
!!--end--
ENDFUNCTION

FUNCTION FactorialLn_A2I4( x , Initialize ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A2.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A2.f90.bdy"
!!--end--
ENDFUNCTION

FUNCTION FactorialLn_A2I8( x , Initialize ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A2.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A2.f90.bdy"
!!--end--
ENDFUNCTION


PURE ELEMENTAL FUNCTION FactorialLn_A1I1( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A1.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A1.f90.bdy"
!!--end--
ENDFUNCTION

PURE ELEMENTAL FUNCTION FactorialLn_A1I2( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A1.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A1.f90.bdy"
!!--end--
ENDFUNCTION

PURE ELEMENTAL FUNCTION FactorialLn_A1I4( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A1.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A1.f90.bdy"
!!--end--
ENDFUNCTION

PURE ELEMENTAL FUNCTION FactorialLn_A1I8( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "08-B-FUN_FactorialLn_A1.f90.hdr"
!!--begin--
INCLUDE "08-B-FUN_FactorialLn_A1.f90.bdy"
!!--end--
ENDFUNCTION

ENDMODULE
