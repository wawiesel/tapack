!!# FUNCTION MODULE: <<FUN_xyANGLE>>
MODULE FUN_xyANGLE


!!## PURPOSE
!! Calculate the angle between two vectors $\theta \in [0,2\pi]$.



!!## MODULE DEPENDENCIES
USE KND_IntrinsicTypes,ONLY: &                !!((01-A-KND_IntrinsicTypes.f90))
  KIND_Rsp,KIND_Rdp

USE PAR_Constants_Rsp ,ONLY: &                !!((02-A-PAR_Constants_Rsp.f90))
  c_0_Rsp           => c_0          , &
  c_1_Rsp           => c_1          , &
  c_2_times_PI_Rsp  => c_2_times_PI , &
  c_180_by_PI_Rsp   => c_180_by_PI

USE PAR_Constants_Rdp,ONLY: &                 !!((02-A-PAR_Constants_Rdp.f90))
  c_0_Rdp            => c_0          , &
  c_1_Rdp            => c_1          , &
  c_2_times_PI_Rdp   => c_2_times_PI , &
  c_180_by_PI_Rdp    => c_180_by_PI

USE FUN_xyDOT      ,ONLY: xyDOT_VV            !!((03-A-FUN_xyDOT.f90))
USE FUN_xyDIRECTION,ONLY: xyDIRECTION_V       !!((04-B-FUN_xyDIRECTION.f90))



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DEFAULT ACCESS
PRIVATE



!!## PROCEDURE OVERLOADING
INTERFACE xyANGLE_VV
 MODULE PROCEDURE xyANGLE_VV_Rsp
 MODULE PROCEDURE xyANGLE_VV_Rdp
END INTERFACE

INTERFACE xyANGLE_UU
 MODULE PROCEDURE xyANGLE_UU_Rsp
 MODULE PROCEDURE xyANGLE_UU_Rdp
END INTERFACE



!!## PUBLIC ACCESS LIST
PUBLIC :: xyANGLE_VV
PUBLIC :: xyANGLE_UU



!!## CONTAINED PROCEDURES
CONTAINS



!!### PURE FUNCTION: <<xyANGLE_UU_Rsp>>
PURE FUNCTION xyANGLE_UU_Rsp( U_a , U_b ) RESULT(t)

!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),PARAMETER :: c_2_times_PI = c_2_times_PI_Rsp
INCLUDE "05-B-FUN_xyANGLE_UU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyANGLE_UU.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: <<xyANGLE_UU_Rdp>>
PURE FUNCTION xyANGLE_UU_Rdp( U_a , U_b ) RESULT(t)

!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
REAL(KIND_R),PARAMETER :: c_2_times_PI = c_2_times_PI_Rdp
INCLUDE "05-B-FUN_xyANGLE_UU.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyANGLE_UU.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: <<xyANGLE_VV_Rsp>>
PURE FUNCTION xyANGLE_VV_Rsp( V_a , V_b ) RESULT(t)

!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyANGLE_VV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyANGLE_VV.f90.bdy"
!!--end--
END FUNCTION



!!### PURE FUNCTION: <<xyANGLE_VV_Rdp>>
PURE FUNCTION xyANGLE_VV_Rdp( V_a , V_b ) RESULT(t)

!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "05-B-FUN_xyANGLE_VV.f90.hdr"
!!--begin--
INCLUDE "05-B-FUN_xyANGLE_VV.f90.bdy"
!!--end--
END FUNCTION


END MODULE
