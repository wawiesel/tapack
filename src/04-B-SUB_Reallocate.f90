!!# MODULE SUBROUTINE <<SUB_Reallocate>>
MODULE SUB_Reallocate

!!## PURPOSE
!! Reallocates a pointer.


!!## MODULES
USE KND_IntrinsicTypes                        !!((01-A-KND_IntrinsicTypes.f90))
USE ISO_varying_string                        !!((03-A-ISO_varying_string.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PROCEDURE OVERLOADING
INTERFACE Reallocate
 MODULE PROCEDURE Reallocate_P1Rsp
 MODULE PROCEDURE Reallocate_P1Rdp
 MODULE PROCEDURE Reallocate_P1Csp
 MODULE PROCEDURE Reallocate_P1Cdp
 MODULE PROCEDURE Reallocate_P1I1
 MODULE PROCEDURE Reallocate_P1I2
 MODULE PROCEDURE Reallocate_P1I4
 MODULE PROCEDURE Reallocate_P1I8
 MODULE PROCEDURE Reallocate_P1L1
 MODULE PROCEDURE Reallocate_P1L2
 MODULE PROCEDURE Reallocate_P1L4
 MODULE PROCEDURE Reallocate_P1S
 MODULE PROCEDURE Reallocate_P1VS
 MODULE PROCEDURE Reallocate_P2Rsp
 MODULE PROCEDURE Reallocate_P2Rdp
 MODULE PROCEDURE Reallocate_P2Csp
 MODULE PROCEDURE Reallocate_P2Cdp
 MODULE PROCEDURE Reallocate_P2I1
 MODULE PROCEDURE Reallocate_P2I2
 MODULE PROCEDURE Reallocate_P2I4
 MODULE PROCEDURE Reallocate_P2I8
 MODULE PROCEDURE Reallocate_P2L1
 MODULE PROCEDURE Reallocate_P2L2
 MODULE PROCEDURE Reallocate_P2L4
 MODULE PROCEDURE Reallocate_P2S
 MODULE PROCEDURE Reallocate_P2VS
 MODULE PROCEDURE Reallocate_P3Rsp
 MODULE PROCEDURE Reallocate_P3Rdp
 MODULE PROCEDURE Reallocate_P3Csp
 MODULE PROCEDURE Reallocate_P3Cdp
 MODULE PROCEDURE Reallocate_P3I1
 MODULE PROCEDURE Reallocate_P3I2
 MODULE PROCEDURE Reallocate_P3I4
 MODULE PROCEDURE Reallocate_P3I8
 MODULE PROCEDURE Reallocate_P3L1
 MODULE PROCEDURE Reallocate_P3L2
 MODULE PROCEDURE Reallocate_P3L4
 MODULE PROCEDURE Reallocate_P3S
 MODULE PROCEDURE Reallocate_P3VS
END INTERFACE


!!## PUBLIC ACCESS LIST
PUBLIC :: Reallocate
PUBLIC :: Reallocate_P1Rsp
PUBLIC :: Reallocate_P1Rdp
PUBLIC :: Reallocate_P1Csp
PUBLIC :: Reallocate_P1Cdp
PUBLIC :: Reallocate_P1I1
PUBLIC :: Reallocate_P1I2
PUBLIC :: Reallocate_P1I4
PUBLIC :: Reallocate_P1I8
PUBLIC :: Reallocate_P1L1
PUBLIC :: Reallocate_P1L2
PUBLIC :: Reallocate_P1L4
PUBLIC :: Reallocate_P1S
PUBLIC :: Reallocate_P1VS
PUBLIC :: Reallocate_P2Rsp
PUBLIC :: Reallocate_P2Rdp
PUBLIC :: Reallocate_P2Csp
PUBLIC :: Reallocate_P2Cdp
PUBLIC :: Reallocate_P2I1
PUBLIC :: Reallocate_P2I2
PUBLIC :: Reallocate_P2I4
PUBLIC :: Reallocate_P2I8
PUBLIC :: Reallocate_P2L1
PUBLIC :: Reallocate_P2L2
PUBLIC :: Reallocate_P2L4
PUBLIC :: Reallocate_P2S
PUBLIC :: Reallocate_P2VS
PUBLIC :: Reallocate_P3Rsp
PUBLIC :: Reallocate_P3Rdp
PUBLIC :: Reallocate_P3Csp
PUBLIC :: Reallocate_P3Cdp
PUBLIC :: Reallocate_P3I1
PUBLIC :: Reallocate_P3I2
PUBLIC :: Reallocate_P3I4
PUBLIC :: Reallocate_P3I8
PUBLIC :: Reallocate_P3L1
PUBLIC :: Reallocate_P3L2
PUBLIC :: Reallocate_P3L4
PUBLIC :: Reallocate_P3S
PUBLIC :: Reallocate_P3VS

!!## MODULE PROCEDURES
CONTAINS

!!### SUBROUTINE <<Reallocate_P1Rsp>>
SUBROUTINE Reallocate_P1Rsp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1R.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1Rdp>>
SUBROUTINE Reallocate_P1Rdp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1R.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1Csp>>
SUBROUTINE Reallocate_P1Csp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1C.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1Cdp>>
SUBROUTINE Reallocate_P1Cdp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1C.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1I1>>
SUBROUTINE Reallocate_P1I1( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1I2>>
SUBROUTINE Reallocate_P1I2( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1I4>>
SUBROUTINE Reallocate_P1I4( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1I8>>
SUBROUTINE Reallocate_P1I8( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1L1>>
SUBROUTINE Reallocate_P1L1( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1L2>>
SUBROUTINE Reallocate_P1L2( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1L4>>
SUBROUTINE Reallocate_P1L4( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P1L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1S>>
SUBROUTINE Reallocate_P1S( data , dn , dnl , fill , noisy )
INCLUDE "04-B-SUB_Reallocate_P1S.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P1VS>>
SUBROUTINE Reallocate_P1VS( data , dn , dnl , fill , noisy )
INCLUDE "04-B-SUB_Reallocate_P1VS.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P1.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P1.f90.bdy"
!!--end--
END SUBROUTINE



!!### SUBROUTINE <<Reallocate_P2Rsp>>
SUBROUTINE Reallocate_P2Rsp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2R.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2Rdp>>
SUBROUTINE Reallocate_P2Rdp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2R.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2Csp>>
SUBROUTINE Reallocate_P2Csp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2C.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2Cdp>>
SUBROUTINE Reallocate_P2Cdp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2C.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2I1>>
SUBROUTINE Reallocate_P2I1( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2I2>>
SUBROUTINE Reallocate_P2I2( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2I4>>
SUBROUTINE Reallocate_P2I4( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2I8>>
SUBROUTINE Reallocate_P2I8( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2L1>>
SUBROUTINE Reallocate_P2L1( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2L2>>
SUBROUTINE Reallocate_P2L2( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2L4>>
SUBROUTINE Reallocate_P2L4( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P2L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2S>>
SUBROUTINE Reallocate_P2S( data , dn , dnl , fill , noisy )
INCLUDE "04-B-SUB_Reallocate_P2S.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P2VS>>
SUBROUTINE Reallocate_P2VS( data , dn , dnl , fill , noisy )
INCLUDE "04-B-SUB_Reallocate_P2VS.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P2.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P2.f90.bdy"
!!--end--
END SUBROUTINE


!!### SUBROUTINE <<Reallocate_P3Rsp>>
SUBROUTINE Reallocate_P3Rsp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3R.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3Rdp>>
SUBROUTINE Reallocate_P3Rdp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3R.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3Csp>>
SUBROUTINE Reallocate_P3Csp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3C.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3Cdp>>
SUBROUTINE Reallocate_P3Cdp( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3C.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3I1>>
SUBROUTINE Reallocate_P3I1( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3I2>>
SUBROUTINE Reallocate_P3I2( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3I4>>
SUBROUTINE Reallocate_P3I4( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3I8>>
SUBROUTINE Reallocate_P3I8( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3I.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3L1>>
SUBROUTINE Reallocate_P3L1( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3L2>>
SUBROUTINE Reallocate_P3L2( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3L4>>
SUBROUTINE Reallocate_P3L4( data , dn , dnl , fill , noisy )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-SUB_Reallocate_P3L.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3S>>
SUBROUTINE Reallocate_P3S( data , dn , dnl , fill , noisy )
INCLUDE "04-B-SUB_Reallocate_P3S.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE

!!### SUBROUTINE <<Reallocate_P3VS>>
SUBROUTINE Reallocate_P3VS( data , dn , dnl , fill , noisy )
INCLUDE "04-B-SUB_Reallocate_P3VS.f90.hdr"
INCLUDE "04-B-SUB_Reallocate_P3.f90.hdr"
!!--begin--
INCLUDE "04-B-SUB_Reallocate_P3.f90.bdy"
!!--end--
END SUBROUTINE



!!### FUNCTION <<getNewLowBound>>
FUNCTION getNewLowBound( lbo , dnl ) RESULT(lb)
INTEGER,INTENT(IN)          :: lbo
INTEGER,INTENT(IN),OPTIONAL :: dnl
INTEGER :: lb
!!--begin

!modify lower bound
IF( PRESENT(dnl) )THEN
 lb = lbo+dnl
ELSE
 lb = lbo
END IF

!!--end
END FUNCTION

!!### FUNCTION <<getNewUppBound>>
FUNCTION getNewUppBound( ubo , dn ) RESULT(ub)
INTEGER,INTENT(IN)          :: ubo
INTEGER,INTENT(IN),OPTIONAL :: dn
INTEGER :: ub
!!--begin

!modify lower bound
IF( PRESENT(dn) )THEN
 ub = ubo+dn
ELSE
 ub = ubo
END IF

!!--end
END FUNCTION


!!### FUNCTION <<getCoreUppBound>>
FUNCTION getCoreUppBound( ubo , ub ) RESULT(uc)
INTEGER,INTENT(IN) :: ubo,ub
INTEGER :: uc
!!--begin
uc = MIN(ub,ubo)
!!--end
END FUNCTION


!!### FUNCTION <<getCoreLowBound>>
FUNCTION getCoreLowBound( lbo , lb ) RESULT(lc)
INTEGER,INTENT(IN) :: lbo,lb
INTEGER :: lc
!!--begin
lc = MAX(lb,lbo)
!!--end
END FUNCTION



!!### FUNCTION <<getFillUppBound>>
FUNCTION getFillUppBound( ubo , ub ) RESULT(uf)
INTEGER,INTENT(IN) :: ubo,ub
INTEGER :: uf
!!--begin
uf = ubo+1
!!--end
END FUNCTION


!!### FUNCTION <<getFillLowBound>>
FUNCTION getFillLowBound( lbo , lb ) RESULT(lf)
INTEGER,INTENT(IN) :: lbo,lb
INTEGER :: lf
!!--begin
lf = lbo-1
!!--end
END FUNCTION



END MODULE
