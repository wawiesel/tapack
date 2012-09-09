!!## MODULE: FUNCTION  FUN_ptr_Sequence
MODULE FUN_ptr_Sequence

!!### PURPOSE
!! Construct an arithmetic sequence for single and
!! double precision reals and default integers, with
!! pointers.



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
!         y => ptr_Sequence( a , b , n )
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
INTERFACE ptr_Sequence
 MODULE PROCEDURE ptr_Sequence_Rsp
 MODULE PROCEDURE ptr_Sequence_Rdp
 MODULE PROCEDURE ptr_Sequence_I
END INTERFACE


!!### GLOBAL VARIABLES
!! @ maximum number of elements allowed to calculate a
!!   direct sequence the easy way.
INTEGER :: MAX_DIRECT_SEQUENCE = 64


!!### PUBLIC ACCESS LIST
PUBLIC :: ptr_Sequence
PUBLIC :: MAX_DIRECT_SEQUENCE


!!## MODULE PROCEDURES
CONTAINS

!!### PURE FUNCTION: ptr_Sequence_Rsp
PURE FUNCTION ptr_Sequence_Rsp( first , increment , n ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_ptr_Sequence_R.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_ptr_Sequence.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: ptr_Sequence_Rdp
PURE FUNCTION ptr_Sequence_Rdp( first , increment , n ) RESULT(y)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_ptr_Sequence_R.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_ptr_Sequence.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: ptr_Sequence_I
PURE FUNCTION ptr_Sequence_I( first , increment , n ) RESULT(y)
INCLUDE "04-A-FUN_ptr_Sequence_I.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_ptr_Sequence.f90.bdy"
!!--end--
END FUNCTION


END MODULE
