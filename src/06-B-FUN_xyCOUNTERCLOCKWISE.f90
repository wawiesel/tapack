MODULE FUN_xyCOUNTERCLOCKWISE
!!#### PURPOSE
!! Determine the counter-clockwise ordering of a chain.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_1_Rsp    => c_1 , & !!((02-A-PAR_Constants_Rsp.f90))
                     c_0_Rsp           => c_0 , &
                     c_PI_Rsp          => c_PI , &
                     c_sqrt_2_by_2_Rsp => c_sqrt_2_by_2
USE PAR_Constants_Rdp,ONLY: c_1_Rdp    => c_1 , & !!((02-A-PAR_Constants_Rdp.f90))
                     c_0_Rdp           => c_0 , &
                     c_PI_Rdp          => c_PI , &
                     c_sqrt_2_by_2_Rdp => c_sqrt_2_by_2

!!#### EXTERNAL PROCEDURES 1
USE FUN_xyANGLE    ,ONLY: xyANGLE_UU              !!((05-B-FUN_xyANGLE.f90))
USE FUN_xyDIRECTION,ONLY: xyDIRECTION_PP          !!((04-B-FUN_xyDIRECTION.f90))

!!#### EXTERNAL PROCEDURES 2
USE FUN_Reverse    ,ONLY: Reverse                 !!((04-A-FUN_Reverse.f90))
USE SUB_Sort_quick ,ONLY: Sort=>Sort_quick        !!((03-A-SUB_Sort_quick.f90))

!!#### GLOBAL USER MODULES
USE USR_Stack                                     !!((04-B-USR_Stack.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyCOUNTERCLOCKWISE_Cn
 MODULE PROCEDURE xyCOUNTERCLOCKWISE_Cn_Rsp
 MODULE PROCEDURE xyCOUNTERCLOCKWISE_Cn_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyCOUNTERCLOCKWISE_Cn


CONTAINS


PURE FUNCTION xyCOUNTERCLOCKWISE_Cn_Rsp( N , Cn , base , anchor , ReverseOrder ) RESULT(order_ccw)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp     !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1           = c_1_Rsp
REAL(KIND_R),PARAMETER :: c_0           = c_0_Rsp
REAL(KIND_R),PARAMETER :: c_PI          = c_PI_Rsp
REAL(KIND_R),PARAMETER :: c_sqrt_2_by_2 = c_sqrt_2_by_2_Rsp
INCLUDE "06-B-FUN_xyCOUNTERCLOCKWISE_Cn.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCOUNTERCLOCKWISE_Cn.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xyCOUNTERCLOCKWISE_Cn_Rdp( N , Cn , base , anchor , ReverseOrder ) RESULT(order_ccw)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp     !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1           = c_1_Rdp
REAL(KIND_R),PARAMETER :: c_0           = c_0_Rdp
REAL(KIND_R),PARAMETER :: c_PI          = c_PI_Rdp
REAL(KIND_R),PARAMETER :: c_sqrt_2_by_2 = c_sqrt_2_by_2_Rdp
INCLUDE "06-B-FUN_xyCOUNTERCLOCKWISE_Cn.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCOUNTERCLOCKWISE_Cn.f90.bdy"
!!--end--
END FUNCTION

END MODULE
