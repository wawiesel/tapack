!!## MODULE: FUNCTION  FUN_Sequence
MODULE FUN_Sequence

!!### PURPOSE
!! Construct an arithmetic sequence for single and
!! double precision reals and default integers.



!!### HISTORY
! 1. [nr] Author=Numerical Recipes
!
! 2. [waw] Reorganizer=William Wieselquist
!          Contact    =william.wieselquist@gmail.com
!          Year       =2006


!!### USAGE
!
!! The call
!
!         y = Sequence( a , b , n )
!
!! returns an arithmetic sequence of length <n>.
!
!         y = [ a , a + b , a + 2*b , ... , a + (n-1)*b ]
!


!!### DETAILS
!! The global variable, <MAX_DIRECT_SEQUENCE>, controls at what value
!! of <n> the evaluation of the arithmetic sequence turns ugly.


!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,KIND_I !!((01-A-KND_IntrinsicTypes.f90))


!!### DEFAULT IMPLICIT
IMPLICIT NONE


!!### DEFAULT ACCESS
PRIVATE


!!### PROCEDURE OVERLOADING
INTERFACE Sequence
 MODULE PROCEDURE Sequence_Rsp
 MODULE PROCEDURE Sequence_Rdp
 MODULE PROCEDURE Sequence_I
END INTERFACE


!!### GLOBAL VARIABLES
!! @ maximum number of elements allowed to calculate a
!!   direct sequence the easy way.
INTEGER :: MAX_DIRECT_SEQUENCE = 64


!!### PUBLIC ACCESS LIST
PUBLIC :: Sequence
PUBLIC :: MAX_DIRECT_SEQUENCE


!!## MODULE PROCEDURES
CONTAINS

!!### PURE FUNCTION: Sequence_Rsp
PURE FUNCTION Sequence_Rsp( first , increment , n ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Sequence_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Sequence.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: Sequence_Rdp
PURE FUNCTION Sequence_Rdp( first , increment , n ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-FUN_Sequence_R.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Sequence.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: Sequence_I
PURE FUNCTION Sequence_I( first , increment , n ) RESULT(y)
INCLUDE "03-A-FUN_Sequence_I.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_Sequence.f90.bdy"
!!--end--
END FUNCTION


END MODULE
