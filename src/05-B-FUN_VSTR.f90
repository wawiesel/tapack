!!# FUNCTION MODULE <<FUN_VSTR>>
MODULE FUN_VSTR

!!## PURPOSE
!! Defines the string creation function, <VSTR> that creates
!! varying strings from intrinsic types.


!!## USAGE
!
!             S = VSTR( X [, FMT , AdjustLeft , AdjustRight ] )
!
!! where <X> is an intrinsic typed variable pointer, <FMT> is an
!! optional format statement, <AdjustLeft> is whether to adjust
!! to the left (default), and <AdjustRight> is whether to
!! adjust to the right.  If the pointer <X> is not associated then
!! the string <NULL(type-kind)>.


!!## METHOD
!! Note: In <VSTR>, efficient internal file WRITE
!! statemtents are used.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes   ,ONLY: KIND_L1,KIND_L2,KIND_L4,& !!((01-A-KND_IntrinsicTypes.f90))
                  KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
                  KIND_Rsp,KIND_Rdp,&
                  KIND_Csp,KIND_Cdp,&
                  KIND_S,KIND_Sfmt

!!## EXTERNAL PARAMETERS
USE PAR_IntrinsicLengths,ONLY: LEN_L1,LEN_L2,LEN_L4,&     !!((02-A-PAR_IntrinsicLengths.f90))
                  LEN_I1,LEN_I2,LEN_I4,LEN_I8,&
                  LEN_Rsp,LEN_Rdp,&
                  LEN_Csp,LEN_Cdp

!!## EXTERNAL PROCEDURES
USE FUN_Error                                             !!((04-A-FUN_Error.f90))
USE SUB_CLEAR                                             !!((04-A-SUB_CLEAR.f90))
USE ISO_varying_string                                    !!((03-A-ISO_varying_string.f90))
USE FUN_Default                                           !!((04-A-FUN_Default.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE VSTR
 MODULE PROCEDURE VSTR_Csp
 MODULE PROCEDURE VSTR_Cdp
 MODULE PROCEDURE VSTR_Rsp
 MODULE PROCEDURE VSTR_Rdp
 MODULE PROCEDURE VSTR_I1
 MODULE PROCEDURE VSTR_I2
 MODULE PROCEDURE VSTR_I4
 MODULE PROCEDURE VSTR_I8
 MODULE PROCEDURE VSTR_L1
 MODULE PROCEDURE VSTR_L2
 MODULE PROCEDURE VSTR_L4
 MODULE PROCEDURE VSTR_S
 MODULE PROCEDURE VSTR_VS
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: VSTR


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: VSTR_Csp
PURE FUNCTION VSTR_Csp(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_Csp
INCLUDE "05-B-FUN_VSTR_C.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: VSTR_Cdp
PURE FUNCTION VSTR_Cdp(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_Cdp
INCLUDE "05-B-FUN_VSTR_C.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: VSTR_Rsp
PURE FUNCTION VSTR_Rsp(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_Rsp
INCLUDE "05-B-FUN_VSTR_R.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: VSTR_Rdp
PURE FUNCTION VSTR_Rdp(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_Rdp
INCLUDE "05-B-FUN_VSTR_R.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: VSTR_I1
PURE FUNCTION VSTR_I1(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_I1
INCLUDE "05-B-FUN_VSTR_I.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: VSTR_I2
PURE FUNCTION VSTR_I2(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_I2
INCLUDE "05-B-FUN_VSTR_I.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: VSTR_I4
PURE FUNCTION VSTR_I4(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_I4
INCLUDE "05-B-FUN_VSTR_I.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: VSTR_I8
PURE FUNCTION VSTR_I8(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_I8
INCLUDE "05-B-FUN_VSTR_I.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: VSTR_L1
PURE FUNCTION VSTR_L1(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_L1
INCLUDE "05-B-FUN_VSTR_L.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: VSTR_L2
PURE FUNCTION VSTR_L2(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_L2
INCLUDE "05-B-FUN_VSTR_L.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: VSTR_L4
PURE FUNCTION VSTR_L4(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_L4
INCLUDE "05-B-FUN_VSTR_L.f90.hdr"
INCLUDE "05-B-FUN_VSTR.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: VSTR_S
PURE FUNCTION VSTR_S(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
INCLUDE "05-B-FUN_VSTR_S.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR.f90.bdy"
!!--end--
END FUNCTION

!!### PURE FUNCTION: VSTR_VS
PURE FUNCTION VSTR_VS(X,FMT,AdjustLeft,AdjustRight) RESULT(VS)
INCLUDE "05-B-FUN_VSTR_VS.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_VSTR_VS.f90.bdy"
!!--end--
END FUNCTION


END MODULE
