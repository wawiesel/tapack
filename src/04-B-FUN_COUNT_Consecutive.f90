!!# MODULE FUNCTION: COUNT_Consecutive
MODULE FUN_COUNT_Consecutive
!!## PURPOSE
!! A simple function to count the consecutive
!! elements in an array, starting from the first element,
!! optional tolerances <tol> and <reltol>, provide extra
!! functionality.


!!## USAGE
!
!    COUNT = COUNT_Consecutive( ARRAY [,tol] [,reltol] )
!
!! where the <IsApprox> function is used to compare consecutive
!! elements of the <ARRAY>.

!!## NOTES
!! The <ARRAY> may be reals or integers
!! but only reals are allowed the optional arguments
!! <tol> and <reltol>


!!## MODULAR KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_I1,KIND_I2,KIND_I4,KIND_I8


!!## MODULE PROCEDURES
USE FUN_IsApprox                                 !!((03-A-FUN_IsApprox.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## TYPE-KIND OVERLOADING
INTERFACE COUNT_Consecutive
 MODULE PROCEDURE COUNT_Consecutive_Rsp
 MODULE PROCEDURE COUNT_Consecutive_Rdp
 MODULE PROCEDURE COUNT_Consecutive_I1
 MODULE PROCEDURE COUNT_Consecutive_I2
 MODULE PROCEDURE COUNT_Consecutive_I4
 MODULE PROCEDURE COUNT_Consecutive_I8
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: COUNT_Consecutive


!!## CONTAINED PROCEDURES
CONTAINS


!!### PURE COUNTING FUNCTION: Count_Consecutive_Rsp
PURE FUNCTION COUNT_Consecutive_Rsp(array,tol,reltol) RESULT(COUNT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_COUNT_Consecutive_R.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_COUNT_Consecutive_R.f90.bdy"
!!--end--
END FUNCTION


!!### PURE COUNTING FUNCTION: Count_Consecutive_Rdp
PURE FUNCTION COUNT_Consecutive_Rdp(array,tol,reltol) RESULT(COUNT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_COUNT_Consecutive_R.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_COUNT_Consecutive_R.f90.bdy"
!!--end--
END FUNCTION


!!### PURE COUNTING FUNCTION: Count_Consecutive_I1
PURE FUNCTION COUNT_Consecutive_I1(array) RESULT(COUNT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.bdy"
!!--end--
END FUNCTION

!!### PURE COUNTING FUNCTION: Count_Consecutive_I2
PURE FUNCTION COUNT_Consecutive_I2(array) RESULT(COUNT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.bdy"
!!--end--
END FUNCTION

!!### PURE COUNTING FUNCTION: Count_Consecutive_I4
PURE FUNCTION COUNT_Consecutive_I4(array) RESULT(COUNT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.bdy"
!!--end--
END FUNCTION

!!### PURE COUNTING FUNCTION: Count_Consecutive_I8
PURE FUNCTION COUNT_Consecutive_I8(array) RESULT(COUNT)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_COUNT_Consecutive_I.f90.bdy"
!!--end--
END FUNCTION
END MODULE
