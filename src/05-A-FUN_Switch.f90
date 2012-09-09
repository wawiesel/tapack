!!## MODULE: FUNCTION  Switch
MODULE FUN_Switch
!!### PURPOSE
!! A simple switch to choose between two values
!! (like a one line IF-ELSE).

!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4,&
              KIND_S

!!### EXTERNAL STANDARDS
USE ISO_varying_string                           !!((03-A-ISO_varying_string.f90))


!!### EXTERNAL PROCEDURES
USE FUN_Error                                    !!((04-A-FUN_Error.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE Switch
 MODULE PROCEDURE Switch_Rsp
 MODULE PROCEDURE Switch_Rdp
 MODULE PROCEDURE Switch_I1
 MODULE PROCEDURE Switch_I2
 MODULE PROCEDURE Switch_I4
 MODULE PROCEDURE Switch_I8
 MODULE PROCEDURE Switch_L1
 MODULE PROCEDURE Switch_L2
 MODULE PROCEDURE Switch_L4
 MODULE PROCEDURE Switch_S
END INTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: Switch

!!## PROCEDURES
CONTAINS

!!### ELEMENTAL FUNCTION: Switch_Rsp
ELEMENTAL FUNCTION Switch_Rsp( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_R.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_Rdp
ELEMENTAL FUNCTION Switch_Rdp( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_R.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_Csp
ELEMENTAL FUNCTION Switch_Csp( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_C.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_Cdp
ELEMENTAL FUNCTION Switch_Cdp( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_C.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_I1
ELEMENTAL FUNCTION Switch_I1( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_I2
ELEMENTAL FUNCTION Switch_I2( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_I4
ELEMENTAL FUNCTION Switch_I4( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_I8
ELEMENTAL FUNCTION Switch_I8( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_I.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_L1
ELEMENTAL FUNCTION Switch_L1( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_L.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch_L.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_L2
ELEMENTAL FUNCTION Switch_L2( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_L.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch_L.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_L4
ELEMENTAL FUNCTION Switch_L4( A , B , Conditional ) RESULT(VAL)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-A-FUN_Switch_L.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch_L.f90.bdy"
!!--end--
END FUNCTION


!!### ELEMENTAL FUNCTION: Switch_S
ELEMENTAL FUNCTION Switch_S( A , B , Conditional ) RESULT(VAL)
INCLUDE "05-A-FUN_Switch_S.f90.hdr"
!!--begin--
INCLUDE "05-A-FUN_Switch.f90.bdy"
!!--end--
END FUNCTION


END MODULE
