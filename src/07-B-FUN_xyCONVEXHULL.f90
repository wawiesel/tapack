!!## MODULE: FUNCTION  xyCONVEXHULL
MODULE FUN_xyCONVEXHULL
!!### PURPOSE
!! Determine the counter-clockwise convex hull
!! ordering from points in a list.
!
!             order = xyCONVEXHULL( N , Px )
!
!!   where N is the number of points to use in the point list,
!
!!                 Px = [ P_1 , P_2 , P_3 , ... , P_N ],
!
!!   where the jth point,
!
!!                P_j = [ x_j , y_j ],
!
!!   is given by the Cartesian x-y coordinates.  The return
!!   value is the counter-clockwise ordered points of
!!   the corresponding convex hull,
!
!!            order = [ k_1 , k_2 , k_3 , ... , k_M ],
!
!!   where k_j corresponds to an index of Px UNLESS k_j==0, then
!!   then the jth point P_j is excluded in order to make
!!   the hull convex.
!!

!!### EXTERNAL KINDS
USE KND_IntrinsicTypes      ,ONLY: KIND_Rsp , KIND_Rdp      !!((01-A-KND_IntrinsicTypes.f90))

!!### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_1_Rsp => c_1 , c_0_Rsp => c_0 !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_1_Rdp => c_1 , c_0_Rdp => c_0 !!((02-A-PAR_Constants_Rdp.f90))

!!### EXTERNAL PROCEDURES
USE FUN_xyDIRECTION       ,ONLY: xyDIRECTION_PP             !!((04-B-FUN_xyDIRECTION.f90))
USE FUN_xyANGLE           ,ONLY: xyANGLE_UU                 !!((05-B-FUN_xyANGLE.f90))
USE FUN_xyROTATE          ,ONLY: xyROTATE_Px                !!((05-B-FUN_xyROTATE.f90))
USE FUN_xyTRANSLATE       ,ONLY: xyTRANSLATE_Px             !!((05-B-FUN_xyTRANSLATE.f90))
USE FUN_xyCOUNTERCLOCKWISE,ONLY: xyCOUNTERCLOCKWISE_Cn      !!((06-B-FUN_xyCOUNTERCLOCKWISE.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE xyCONVEXHULL_Px
 MODULE PROCEDURE xyCONVEXHULL_Px_Rsp
 MODULE PROCEDURE xyCONVEXHULL_Px_Rdp
ENDINTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: xyCONVEXHULL_Px


!!### MODULE PROCEDURES
CONTAINS


!!### PURE FUNCTION: xyCONVEXHULL_Px_Rsp
PURE FUNCTION xyCONVEXHULL_Px_Rsp( N , Px ) RESULT(order_cvh)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R = KIND_Rsp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rsp
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "07-B-FUN_xyCONVEXHULL_Px.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyCONVEXHULL_Px.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: xyCONVEXHULL_Px_Rdp
PURE FUNCTION xyCONVEXHULL_Px_Rdp( N , Px ) RESULT(order_cvh)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R = KIND_Rdp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rdp
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "07-B-FUN_xyCONVEXHULL_Px.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyCONVEXHULL_Px.f90.bdy"
!!--end--
END FUNCTION


END MODULE
