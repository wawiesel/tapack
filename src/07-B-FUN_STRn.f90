!!# FUNCTION MODULE: <<FUN_STRn>>
MODULE FUN_STRn

!!## PURPOSE
!! Defines another string creation function, <STRn>, which
!! takes a pointer argument and returns <NULL(type-kind)>
!! in  the case the pointer is not associated.

!!## USAGE
!
!             S = STRn( X [, FMT , AdjustLeft , AdjustRight ] )
!
!! where <X> is an intrinsic typed variable pointer of rank 0 or 1, <FMT> is an
!! optional format statement, <AdjustLeft> is whether to adjust
!! to the left (default), and <AdjustRight> is whether to
!! adjust to the right.  If the pointer <X> is not associated then
!! the string <NULL(type-kind)>.

!!## METHOD
!! The check if <X> is associated is performed and if it is, <FUN_STR>
!! is used to generate the output string.  Note: In <FUN_STR>,
!! efficient internal file WRITE statemtents are used.

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
USE FUN_ptr_STR                                           !!((06-B-FUN_ptr_STR.f90))
USE FUN_Switch                                            !!((05-A-FUN_Switch.f90))
USE ISO_varying_string                                    !!((03-A-ISO_varying_string.f90))
USE FUN_Default                                           !!((04-A-FUN_Default.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE STRn
 MODULE PROCEDURE STRn_P0Csp
 MODULE PROCEDURE STRn_P0Cdp
 MODULE PROCEDURE STRn_P0Rsp
 MODULE PROCEDURE STRn_P0Rdp
 MODULE PROCEDURE STRn_P0I1
 MODULE PROCEDURE STRn_P0I2
 MODULE PROCEDURE STRn_P0S
 MODULE PROCEDURE STRn_P0I4
 MODULE PROCEDURE STRn_P0I8
 MODULE PROCEDURE STRn_P0L1
 MODULE PROCEDURE STRn_P0L2
 MODULE PROCEDURE STRn_P0L4
 MODULE PROCEDURE STRn_P0VS
 MODULE PROCEDURE STRn_P1Csp
 MODULE PROCEDURE STRn_P1Cdp
 MODULE PROCEDURE STRn_P1Rsp
 MODULE PROCEDURE STRn_P1Rdp
 MODULE PROCEDURE STRn_P1I1
 MODULE PROCEDURE STRn_P1I2
 MODULE PROCEDURE STRn_P1I4
 MODULE PROCEDURE STRn_P1I8
 MODULE PROCEDURE STRn_P1L1
 MODULE PROCEDURE STRn_P1L2
 MODULE PROCEDURE STRn_P1L4
 MODULE PROCEDURE STRn_P1S
 MODULE PROCEDURE STRn_P1VS
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: STRn
PUBLIC :: MAX_STRING_LENGTH

!!## MODULE PROCEDURES
CONTAINS


!!### FUNCTION: STRn_P0Csp
FUNCTION STRn_P0Csp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Csp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Csp"
INCLUDE "07-B-FUN_STRn_P0C.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION

!!### FUNCTION: STRn_P0Cdp
FUNCTION STRn_P0Cdp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Cdp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Cdp"
INCLUDE "07-B-FUN_STRn_P0C.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0Rsp
FUNCTION STRn_P0Rsp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Rsp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Rsp"
INCLUDE "07-B-FUN_STRn_P0R.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION

!!### FUNCTION: STRn_P0Rdp
FUNCTION STRn_P0Rdp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Rdp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Rdp"
INCLUDE "07-B-FUN_STRn_P0R.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION: STRn_P0I1
FUNCTION STRn_P0I1(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I1
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I1"
INCLUDE "07-B-FUN_STRn_P0I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0I2
FUNCTION STRn_P0I2(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I2
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I2"
INCLUDE "07-B-FUN_STRn_P0I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0I4
FUNCTION STRn_P0I4(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I4
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I4"
INCLUDE "07-B-FUN_STRn_P0I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0I8
FUNCTION STRn_P0I8(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I8
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I8"
INCLUDE "07-B-FUN_STRn_P0I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION: STRn_P0L1
FUNCTION STRn_P0L1(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_NULL = 8
INTEGER,PARAMETER :: LEN_S    = LEN_L1
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="L1"
INCLUDE "07-B-FUN_STRn_P0L.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0L2
FUNCTION STRn_P0L2(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_NULL = 8
INTEGER,PARAMETER :: LEN_S    = LEN_L2
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="L2"
INCLUDE "07-B-FUN_STRn_P0L.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0L4
FUNCTION STRn_P0L4(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_NULL = 8
INTEGER,PARAMETER :: LEN_S    = LEN_L4
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="L4"
INCLUDE "07-B-FUN_STRn_P0L.f90.hdr"
INCLUDE "07-B-FUN_STRn_P0.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0S
FUNCTION STRn_P0S(Sin,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL PARAMETERS
INTEGER     ,PARAMETER :: LEN_NULL = 7
CHARACTER(*),PARAMETER :: TYPE_KIND="S"
INCLUDE "07-B-FUN_STRn_P0S.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0S.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P0VS
FUNCTION STRn_P0VS(VSin,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL PARAMETERS
INTEGER     ,PARAMETER :: LEN_NULL = 8
CHARACTER(*),PARAMETER :: TYPE_KIND="VS"
INCLUDE "07-B-FUN_STRn_P0VS.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P0VS.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION: STRn_P1Csp
FUNCTION STRn_P1Csp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Csp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Csp"
INCLUDE "07-B-FUN_STRn_P1C.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION

!!### FUNCTION: STRn_P1Cdp
FUNCTION STRn_P1Cdp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Cdp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Cdp"
INCLUDE "07-B-FUN_STRn_P1C.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P1Rsp
FUNCTION STRn_P1Rsp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Rsp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Rsp"
INCLUDE "07-B-FUN_STRn_P1R.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION

!!### FUNCTION: STRn_P1Rdp
FUNCTION STRn_P1Rdp(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_Rdp
INTEGER,PARAMETER :: LEN_NULL = 9
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="Rdp"
INCLUDE "07-B-FUN_STRn_P1R.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION: STRn_P1I1
FUNCTION STRn_P1I1(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I1
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I1"
INCLUDE "07-B-FUN_STRn_P1I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P1I2
FUNCTION STRn_P1I2(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I2
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I2"
INCLUDE "07-B-FUN_STRn_P1I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P1I4
FUNCTION STRn_P1I4(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I4
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I4"
INCLUDE "07-B-FUN_STRn_P1I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P1I8
FUNCTION STRn_P1I8(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_I8
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="I8"
INCLUDE "07-B-FUN_STRn_P1I.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION: STRn_P1L1
FUNCTION STRn_P1L1(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S=LEN_L1
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="L1"
INCLUDE "07-B-FUN_STRn_P1L.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P1L2
FUNCTION STRn_P1L2(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_S    = LEN_L2
INTEGER,PARAMETER :: LEN_NULL = 8
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="L2"
INCLUDE "07-B-FUN_STRn_P1L.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P1L4
FUNCTION STRn_P1L4(X,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4              !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL LENGTHS
INTEGER,PARAMETER :: LEN_NULL = 8
INTEGER,PARAMETER :: LEN_S    = LEN_L4
!!#### LOCAL PARAMETERS
CHARACTER(*),PARAMETER :: TYPE_KIND="L4"
INCLUDE "07-B-FUN_STRn_P1L.f90.hdr"
INCLUDE "07-B-FUN_STRn_P1.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION: STRn_P1S
FUNCTION STRn_P1S(Sin,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: LEN_NULL = 7
CHARACTER(*),PARAMETER :: TYPE_KIND="S"
INCLUDE "07-B-FUN_STRn_P1S.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1S.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: STRn_P1VS
FUNCTION STRn_P1VS(VSin,FMT,AdjustLeft,AdjustRight) RESULT(S)
!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: LEN_NULL = 8
CHARACTER(*),PARAMETER :: TYPE_KIND="VS"
INCLUDE "07-B-FUN_STRn_P1VS.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_STRn_P1VS.f90.bdy"
!!--end--
END FUNCTION



END MODULE
