!!## BINARY FUNCTION/OPERATOR: Concatenate
MODULE BOP_Concatenate

!!### PURPOSE
!! Defines binary operator <//> for numeric arrays, to
!! combine one array with another, <C = A//B>.



!!### USAGE
!!
!! The operator form is
!
!     C = A//B
!
!! The functional form is
!
!     C = Concatenate( A , B )
!



!!### COMMENT
!! Using <//> instead of array constructors <(/A,B/)>
!! offers a simpler notation for the simple low-level operation
!! of concatenation of arrays.



!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: &                !!((01-A-KND_IntrinsicTypes.f90))
  KIND_L1,KIND_L2,KIND_L4,&
  KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
  KIND_Rsp,KIND_Rdp,&
  KIND_Csp,KIND_Cdp



!!### HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!### DEFAULT IMPLICIT
IMPLICIT NONE



!!### DEFAULT ACCESS
PRIVATE



!!### FUNCTION DEFINITION
INTERFACE Concatenate
 MODULE PROCEDURE Concatenate_Rsp
 MODULE PROCEDURE Concatenate_Rdp
 MODULE PROCEDURE Concatenate_Csp
 MODULE PROCEDURE Concatenate_Cdp
 MODULE PROCEDURE Concatenate_I1
 MODULE PROCEDURE Concatenate_I2
 MODULE PROCEDURE Concatenate_I4
 MODULE PROCEDURE Concatenate_I8
 MODULE PROCEDURE Concatenate_L1
 MODULE PROCEDURE Concatenate_L2
 MODULE PROCEDURE Concatenate_L4
END INTERFACE



!!### OPERATOR DEFINITION
INTERFACE OPERATOR(//)
 MODULE PROCEDURE Concatenate_Rsp
 MODULE PROCEDURE Concatenate_Rdp
 MODULE PROCEDURE Concatenate_Csp
 MODULE PROCEDURE Concatenate_Cdp
 MODULE PROCEDURE Concatenate_I1
 MODULE PROCEDURE Concatenate_I2
 MODULE PROCEDURE Concatenate_I4
 MODULE PROCEDURE Concatenate_I8
 MODULE PROCEDURE Concatenate_L1
 MODULE PROCEDURE Concatenate_L2
 MODULE PROCEDURE Concatenate_L4
END INTERFACE



!!#### PUBLIC ACCESS LIST
PUBLIC :: OPERATOR(//)
PUBLIC :: Concatenate


CONTAINS


PURE FUNCTION Concatenate_Rsp(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_R.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION Concatenate_Rdp(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_R.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION


PURE FUNCTION Concatenate_Csp(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_C.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION Concatenate_Cdp(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_C.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION


PURE FUNCTION Concatenate_I1(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_I.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION Concatenate_I2(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_I.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION Concatenate_I4(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_I.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION Concatenate_I8(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_I.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION


PURE FUNCTION Concatenate_L1(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1  !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_L.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION Concatenate_L2(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2  !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_L.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION

PURE FUNCTION Concatenate_L4(A,B) RESULT(C)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4  !!((01-A-KND_IntrinsicTypes.f90))

INCLUDE "03-A-BOP_Concatenate_L.f90.hdr"
!!--begin--
INCLUDE "03-A-BOP_Concatenate.f90.bdy"
!!--end--
ENDFUNCTION


ENDMODULE
