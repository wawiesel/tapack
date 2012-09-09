!!## MODULE: FUNCTION  FUN_SIZEa
MODULE FUN_SIZEa
!!### PURPOSE
!! A companion function to the ERROR function.  It returns
!! the number of elements in a rank 1 array counted sequentially
!! from the last element that ARE NOT equal to the ERROR number
!! for that type-kind.

!!### USAGE
!
!   N = SIZEa( ARRAY )
!

!!### EXAMPLES
!!
!   ARRAY = [0,1,2,-HUGE(ARRAY),-HUGE(ARRAY)]
!   SIZEa(ARRAY) => 3
!!
!   ARRAY = [2,3]
!   SIZEa(ARRAY) => 2
!!
!   ARRAY = [-19,-HUGE(ARRAY),32]
!   SIZEa(ARRAY) => 3
!!

!!### COMMENT
!! If you initialize arrays with <ARRAY=ERROR(ARRAY)>, and
!! you use up elements sequentially, then <SIZEa(ARRAY)> will
!! tell you how many elements you have used so far.  You could
!! then <Reallocate> the array to the correct size:
!!
!    N = SIZEa(ARRAY)
!    CALL Reallocate( ARRAY , N-SIZE(ARRAY) )
!!


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I,KIND_I1,KIND_I2,KIND_I4,KIND_I8,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_R,KIND_Rsp,KIND_Rdp,&
              KIND_C,KIND_Csp,KIND_Cdp

!!### GLOBAL FUNCTIONS
USE FUN_IsError                                                       !!((05-A-FUN_IsError.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE SIZEa
 MODULE PROCEDURE SIZEa_Csp
 MODULE PROCEDURE SIZEa_Cdp
 MODULE PROCEDURE SIZEa_Rsp
 MODULE PROCEDURE SIZEa_Rdp
 MODULE PROCEDURE SIZEa_I1
 MODULE PROCEDURE SIZEa_I2
 MODULE PROCEDURE SIZEa_I4
 MODULE PROCEDURE SIZEa_I8
 MODULE PROCEDURE SIZEa_S
 MODULE PROCEDURE SIZEa_VS
END INTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: SIZEa


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: SIZEa_Rsp
PURE FUNCTION SIZEa_Rsp(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_R.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_Rdp
PURE FUNCTION SIZEa_Rdp(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_R.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_Csp
PURE FUNCTION SIZEa_Csp(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_C.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_Cdp
PURE FUNCTION SIZEa_Cdp(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_C.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_I1
PURE FUNCTION SIZEa_I1(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_I.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_I2
PURE FUNCTION SIZEa_I2(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_I.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_I4
PURE FUNCTION SIZEa_I4(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_I.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_I8
PURE FUNCTION SIZEa_I8(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_I.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: SIZEa_S
PURE FUNCTION SIZEa_S(ARRAY) RESULT(N)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S                                   !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-FUN_SIZEa_S.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: SIZEa_VS
PURE FUNCTION SIZEa_VS(ARRAY) RESULT(N)
USE ISO_varying_string                                                !!((03-A-ISO_varying_string.f90))
INCLUDE "06-B-FUN_SIZEa_VS.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_SIZEa.f90.bdy"
!!--end--
END FUNCTION

END MODULE
