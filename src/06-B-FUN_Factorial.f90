!!# FUNCTION MODULE: <FUN_Factorial>
MODULE FUN_Factorial

!!## PURPOSE
!! Defines function Factorial(k) which returns a real double precision
!! for integer k.  If the factorial does not exist Infinity is returned.

!!## DETAILS
!! @ The kind of real Factorial(k) is equal to the kind of integer k.
!! @ A check is made to make sure k has a factorial that is less than HUGE(k).

!!## PARAMETERS
!! @ kind of the real function return value
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp         !!((01-A-KND_IntrinsicTypes.f90))

!!## MODULES
USE FUN_FactorialInt,ONLY: MAX_I4,MAX_I8,FactorialInt !!((05-B-FUN_FactorialInt.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## FUNCTION OVERLOADING
INTERFACE Factorial
 MODULE PROCEDURE Factorial_I4
 MODULE PROCEDURE Factorial_I8
ENDINTERFACE
PUBLIC :: Factorial

CONTAINS

PURE ELEMENTAL FUNCTION Factorial_I4( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4          !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: MAX   = MAX_I4
INCLUDE "06-B-FUN_Factorial.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_Factorial.f90.bdy"
!!--end--
ENDFUNCTION


PURE ELEMENTAL FUNCTION Factorial_I8( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8          !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: MAX   = MAX_I8
INCLUDE "06-B-FUN_Factorial.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_Factorial.f90.bdy"
!!--end--
ENDFUNCTION


ENDMODULE
