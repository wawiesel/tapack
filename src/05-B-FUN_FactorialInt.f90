
!!# FUNCTION MODULE: <FUN_FactorialInt>
MODULE FUN_FactorialInt

!!## PURPOSE
!! Defines function FactorialInt(k) which returns a integer for integer k.
!! If the factorial is beyond the range of the integer k then 0 is returned.

!!## DETAILS
!! @ The kind of FactorialInt(k) is equal to the kind of k.
!! @ A check is made to make sure k has a factorial that is less than HUGE(k).

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I1,KIND_I2,KIND_I4,KIND_I8 !!((01-A-KND_IntrinsicTypes.f90))

!!## GLOBAL PROCEDURE
USE FUN_Error                                                !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!! @ maximum integer-only factorial
INTEGER(KIND_I1),PARAMETER :: MAX_I1 = 5_KIND_I1  !maximum integer factorial for INTEGER(1)
INTEGER(KIND_I2),PARAMETER :: MAX_I2 = 7_KIND_I2  !maximum integer factorial for INTEGER(2)
INTEGER(KIND_I4),PARAMETER :: MAX_I4 = 13_KIND_I4 !maximum integer factorial for INTEGER(4)
INTEGER(KIND_I8),PARAMETER :: MAX_I8 = 20_KIND_I8 !maximum integer factorial for INTEGER(8)
PUBLIC :: MAX_I1,MAX_I2,MAX_I4,MAX_I8

!!## FUNCTION OVERLOADING
INTERFACE FactorialInt
 MODULE PROCEDURE FactorialInt_I1
 MODULE PROCEDURE FactorialInt_I2
 MODULE PROCEDURE FactorialInt_I4
 MODULE PROCEDURE FactorialInt_I8
END INTERFACE

!!## PUBLIC ACCESS
PUBLIC :: FactorialInt

!!## CONTAINED PROCEDURES
CONTAINS


PURE ELEMENTAL FUNCTION FactorialInt_I1( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                 !!((01-A-KND_IntrinsicTypes.f90))
INTEGER(KIND_I),PARAMETER :: MAX = MAX_I1
INCLUDE "05-B-FUN_FactorialInt.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_FactorialInt.f90.bdy"
!!--end--
ENDFUNCTION


PURE ELEMENTAL FUNCTION FactorialInt_I2( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                 !!((01-A-KND_IntrinsicTypes.f90))
INTEGER(KIND_I),PARAMETER :: MAX = MAX_I2
INCLUDE "05-B-FUN_FactorialInt.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_FactorialInt.f90.bdy"
!!--end--
ENDFUNCTION


PURE ELEMENTAL FUNCTION FactorialInt_I4( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                 !!((01-A-KND_IntrinsicTypes.f90))
INTEGER(KIND_I),PARAMETER :: MAX = MAX_I4
INCLUDE "05-B-FUN_FactorialInt.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_FactorialInt.f90.bdy"
!!--end--
ENDFUNCTION


PURE ELEMENTAL FUNCTION FactorialInt_I8( x ) RESULT(y)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                 !!((01-A-KND_IntrinsicTypes.f90))
INTEGER(KIND_I),PARAMETER :: MAX = MAX_I8
INCLUDE "05-B-FUN_FactorialInt.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_FactorialInt.f90.bdy"
!!--end--
ENDFUNCTION


ENDMODULE
