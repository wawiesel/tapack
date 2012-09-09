!!# FUNCTION MODULE: <IsError>
MODULE FUN_IsError

!!## PURPOSE
!! Check if a variable has a special error value.


!!## USAGE
!! Returns <.TRUE.> if <x> is the error number for the
!! type of <x>.
!
!                       TorF = IsError( x )
!


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R,KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_I,KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_C,KIND_Csp,KIND_Cdp,&
              KIND_S


!!## EXTERNAL STANDARDS
USE ISO_varying_string                                  !!((03-A-ISO_varying_string.f90))

!!## GLOBAL FUNCTIONS
USE FUN_Error                                           !!((04-A-FUN_Error.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE IsError
 MODULE PROCEDURE IsError_Csp
 MODULE PROCEDURE IsError_Cdp
 MODULE PROCEDURE IsError_Rsp
 MODULE PROCEDURE IsError_Rdp
 MODULE PROCEDURE IsError_I1
 MODULE PROCEDURE IsError_I2
 MODULE PROCEDURE IsError_I4
 MODULE PROCEDURE IsError_I8
 MODULE PROCEDURE IsError_VS
 MODULE PROCEDURE IsError_S
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: IsError


!!## MODULE PROCEDURES
CONTAINS


!!### ELEMENTAL FUNCTION: <IsError_Rsp>
ELEMENTAL FUNCTION IsError_Rsp(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_R.f90.hdr"
!
IsError = B==ERROR_Rsp
!
END FUNCTION


!!### ELEMENTAL FUNCTION: <IsError_Rdp>
ELEMENTAL FUNCTION IsError_Rdp(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_R.f90.hdr"
!
IsError = B==ERROR_Rdp
!
END FUNCTION

!!### ELEMENTAL FUNCTION: <IsError_Csp>
ELEMENTAL FUNCTION IsError_Csp(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_C.f90.hdr"
!
IsError = B==ERROR_Csp
!
END FUNCTION

!!### ELEMENTAL FUNCTION: <IsError_Cdp>
ELEMENTAL FUNCTION IsError_Cdp(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp           !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_C.f90.hdr"
!
IsError = B==ERROR_Cdp
!
END FUNCTION

!!### ELEMENTAL FUNCTION: <IsError_I1>
ELEMENTAL FUNCTION IsError_I1(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_I.f90.hdr"
!
IsError = B==ERROR_I1
!
END FUNCTION

!!### ELEMENTAL FUNCTION: <IsError_I2>
ELEMENTAL FUNCTION IsError_I2(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_I.f90.hdr"
!
IsError = B==ERROR_I2
!
END FUNCTION

!!### ELEMENTAL FUNCTION: <IsError_I4>
ELEMENTAL FUNCTION IsError_I4(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_I.f90.hdr"
!
IsError = B==ERROR_I4
!
END FUNCTION

!!### ELEMENTAL FUNCTION: <IsError_I8>
ELEMENTAL FUNCTION IsError_I8(B) RESULT(IsError)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8            !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_IsError_I.f90.hdr"
!
IsError = B==ERROR_I8
!
END FUNCTION


!!### ELEMENTAL FUNCTION: <IsError_S>
ELEMENTAL FUNCTION IsError_S(B) RESULT(IsError)
INCLUDE "05-A-FUN_IsError_S.f90.hdr"
!
IsError = B==ERROR_S
!
END FUNCTION


!!### ELEMENTAL FUNCTION: <IsError_VS>
ELEMENTAL FUNCTION IsError_VS(B) RESULT(IsError)
INCLUDE "05-A-FUN_IsError_VS.f90.hdr"
!
IsError = B==ERROR_S
!
END FUNCTION

END MODULE
