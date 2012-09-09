!!#FUNCTION MODULE: Default
MODULE FUN_Default

!!##PURPOSE
!! Make a simple way to choose the default value of an <OPTIONAL>
!! argument or the actual value, if it is <PRESENT>.



!!##USAGE
!
!  VAL = Default( DEFAULT_VAL , OPTIONAL_VAL [, OVERRIDE ] )
!
!! where <DEFAULT_VAL> is the default value, <OPTIONAL_VAL>
!! is the optional value (that may not be present), and
!! <OVERRIDE> is the optional flag to use the <DEFAULT_VAL>
!! no matter what (override the optional value).



!!##COMMENT
!! You may not want to use this in very low-level routines,
!! because it may prevent some optimization.



!!##EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_Csp,KIND_Cdp,&
                             KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
                             KIND_L1,KIND_L2,KIND_L4,&
                             KIND_S



!!##STANDARDS
USE ISO_varying_string                           !!((03-A-ISO_varying_string.f90))



!!##HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 104.2006
!         Contact  = william.wieselquist AT gmail.com



!!##DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!##PROCEDURE OVERLOADING
INTERFACE Default
 MODULE PROCEDURE Default_Rsp
 MODULE PROCEDURE Default_Rdp
 MODULE PROCEDURE Default_I1
 MODULE PROCEDURE Default_I2
 MODULE PROCEDURE Default_I4
 MODULE PROCEDURE Default_I8
 MODULE PROCEDURE Default_L1
 MODULE PROCEDURE Default_L2
 MODULE PROCEDURE Default_L4
 MODULE PROCEDURE Default_S
 MODULE PROCEDURE Default_VS
END INTERFACE



!!##PUBLIC ACCESS LIST
PUBLIC :: Default



!!##MODULE PROCEDURES
CONTAINS



!!###PURE ELEMENTAL FUNCTION: Default_Rsp
PURE ELEMENTAL FUNCTION Default_Rsp( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_R.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_Rdp
PURE ELEMENTAL FUNCTION Default_Rdp( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_R.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_Csp
PURE ELEMENTAL FUNCTION Default_Csp( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_C.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_Cdp
PURE ELEMENTAL FUNCTION Default_Cdp( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_C.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_I1
PURE ELEMENTAL FUNCTION Default_I1( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_I.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_I2
PURE ELEMENTAL FUNCTION Default_I2( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_I.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_I4
PURE ELEMENTAL FUNCTION Default_I4( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_I.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_I8
PURE ELEMENTAL FUNCTION Default_I8( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_I.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_L1
PURE ELEMENTAL FUNCTION Default_L1( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_L.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_L2
PURE ELEMENTAL FUNCTION Default_L2( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_L.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_L4
PURE ELEMENTAL FUNCTION Default_L4( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Default_L.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_S
PURE ELEMENTAL FUNCTION Default_S( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
INCLUDE "04-A-FUN_Default_S.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


!!###PURE ELEMENTAL FUNCTION: Default_VS
PURE ELEMENTAL FUNCTION Default_VS( DEFAULT_VAL , OPTIONAL_VAL , OVERRIDE ) RESULT(VAL)
INCLUDE "04-A-FUN_Default_VS.f90.hdr"
INCLUDE "04-A-FUN_Default.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Default.f90.bdy"
!!--end--
END FUNCTION


END MODULE
