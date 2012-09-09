MODULE FUN_IsWarning
!!#### PURPOSE
!! Provide the check if a scalar or array has
!! any Warnings are present.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I,KIND_I1,KIND_I2,KIND_I4,KIND_I8,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_R,KIND_Rsp,KIND_Rdp,&
              KIND_C,KIND_Csp,KIND_Cdp

!!#### GLOBAL FUNCTIONS
USE FUN_Warning                                                       !!((04-B-FUN_Warning.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE IsWarning
 MODULE PROCEDURE IsWarning_Csp
 MODULE PROCEDURE IsWarning_Cdp
 MODULE PROCEDURE IsWarning_Rsp
 MODULE PROCEDURE IsWarning_Rdp
 MODULE PROCEDURE IsWarning_I1
 MODULE PROCEDURE IsWarning_I2
 MODULE PROCEDURE IsWarning_I4
 MODULE PROCEDURE IsWarning_I8
END INTERFACE

!!#### PUBLIC ACCESS LIST
!! @ function
PUBLIC :: IsWarning


CONTAINS


PURE ELEMENTAL FUNCTION IsWarning_Rsp(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_R.f90.hdr"
!
IsWarning = B==Warning_Rsp
!
END FUNCTION

PURE ELEMENTAL FUNCTION IsWarning_Rdp(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_R.f90.hdr"
!
IsWarning = B==Warning_Rdp
!
END FUNCTION

PURE ELEMENTAL FUNCTION IsWarning_Csp(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_C.f90.hdr"
!
IsWarning = B==Warning_Csp
!
END FUNCTION

PURE ELEMENTAL FUNCTION IsWarning_Cdp(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp                         !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_C.f90.hdr"
!
IsWarning = B==Warning_Cdp
!
END FUNCTION


PURE ELEMENTAL FUNCTION IsWarning_I1(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_I.f90.hdr"
!
IsWarning = B==Warning_I1
!
END FUNCTION

PURE ELEMENTAL FUNCTION IsWarning_I2(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_I.f90.hdr"
!
IsWarning = B==Warning_I2
!
END FUNCTION

PURE ELEMENTAL FUNCTION IsWarning_I4(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_I.f90.hdr"
!
IsWarning = B==Warning_I4
!
END FUNCTION

PURE ELEMENTAL FUNCTION IsWarning_I8(B) RESULT(IsWarning)
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8                          !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_IsWarning_I.f90.hdr"
!
IsWarning = B==Warning_I8
!
END FUNCTION

END MODULE
