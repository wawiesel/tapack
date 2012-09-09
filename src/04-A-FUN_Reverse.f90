!!# FUNCTION MODULE: <FUN_Reverse>
MODULE FUN_Reverse

!!## PURPOSE
!! Reverse the order of elements in a 1D array of intrinsic type.

!!## USAGE
!
!                 z = Reverse(y)
!
!! where <y> and <z> are 1D arrays of intrinsic type.
!! Returns reversed <y>, as < z(i) = y(N-i+1), i=1,..,N>, where
!! <N=SIZE(y)>.


!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4,&
              KIND_S
USE ISO_varying_string                           !!((03-A-ISO_varying_string.f90))


!!## AUTHOR
!! William A. Wieselquist | william.wieselquist@gmail.com


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE Reverse
 MODULE PROCEDURE Reverse_A1Rsp
 MODULE PROCEDURE Reverse_A1Rdp
 MODULE PROCEDURE Reverse_A1Csp
 MODULE PROCEDURE Reverse_A1Cdp
 MODULE PROCEDURE Reverse_A1I1
 MODULE PROCEDURE Reverse_A1I2
 MODULE PROCEDURE Reverse_A1I4
 MODULE PROCEDURE Reverse_A1I8
 MODULE PROCEDURE Reverse_A1L1
 MODULE PROCEDURE Reverse_A1L2
 MODULE PROCEDURE Reverse_A1L4
 MODULE PROCEDURE Reverse_A1S
 MODULE PROCEDURE Reverse_A1VS
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: Reverse


!!## MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: Reverse_A1S
PURE FUNCTION Reverse_A1S( y ) RESULT( z )
INCLUDE "04-A-FUN_Reverse_A1S.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1VS
PURE FUNCTION Reverse_A1VS( y ) RESULT( z )
INCLUDE "04-A-FUN_Reverse_A1VS.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1L1
PURE FUNCTION Reverse_A1L1( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1L.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1L2
PURE FUNCTION Reverse_A1L2( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1L.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1L4
PURE FUNCTION Reverse_A1L4( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1L.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1I1
PURE FUNCTION Reverse_A1I1( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1I.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1I2
PURE FUNCTION Reverse_A1I2( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1I.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1I4
PURE FUNCTION Reverse_A1I4( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1I.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1I8
PURE FUNCTION Reverse_A1I8( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1I.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1Rsp
PURE FUNCTION Reverse_A1Rsp( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1R.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1Rdp
PURE FUNCTION Reverse_A1Rdp( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1R.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1Csp
PURE FUNCTION Reverse_A1Csp( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1C.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: Reverse_A1Cdp
PURE FUNCTION Reverse_A1Cdp( y ) RESULT( z )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-FUN_Reverse_A1C.f90.hdr"
!!--begin--
INCLUDE "04-A-FUN_Reverse_A1.f90.bdy"
!!--end--
END FUNCTION


END MODULE
