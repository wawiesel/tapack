MODULE FUN_xyCONTIGUOUS
!!#### PURPOSE
!! Return the contiguous ordering of line segments.

!!#### METHOD
!! The ordering begins with the first segment, <AB> and sets the
!! order to <order(1)=1>.
!!   A-----------B                C"
!!                   B"           |
!!                     \          |
!!                       \        D
!!                         C
!! The second point on the first segment, <B=Lsx(:,2,1)> is the
!! first point matched.  If the first point on the second segment,
!! <B"=Lsx(:,1,2> matches B then the second contiguous segment has been found,
!! <order(2)=2>.  If instead, <B> matches <C> then <order(2)=-2>

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp , KIND_Rdp              !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp  ,ONLY: c_1_Rsp => c_1 , c_0_Rsp => c_0 !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp  ,ONLY: c_1_Rdp => c_1 , c_0_Rdp => c_0 !!((02-A-PAR_Constants_Rdp.f90))
USE PAR_ComputationalGeometry,ONLY: xySHAPE_P,xySHAPE_Ls      !!((02-A-PAR_ComputationalGeometry.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyDIST,ONLY: xyDIST_PP                                !!((04-B-FUN_xyDIST.f90))
USE FUN_Error                                                 !!((04-A-FUN_Error.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyCONTIGUOUS_Lsx
 MODULE PROCEDURE xyCONTIGUOUS_Lsx_Rsp
 MODULE PROCEDURE xyCONTIGUOUS_Lsx_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xyCONTIGUOUS_Lsx


CONTAINS


PURE FUNCTION xyCONTIGUOUS_Lsx_Rsp( N , Lsx , tol , Noisy ) RESULT(order)
INTEGER    ,PARAMETER :: KIND_R = KIND_Rsp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rsp
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "06-B-FUN_xyCONTIGUOUS_Lsx.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCONTIGUOUS_Lsx.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xyCONTIGUOUS_Lsx_Rdp( N , Lsx , tol , Noisy ) RESULT(order)
!!#### LOCAL PARAMETERS
INTEGER    ,PARAMETER :: KIND_R = KIND_Rdp
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rdp
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "06-B-FUN_xyCONTIGUOUS_Lsx.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyCONTIGUOUS_Lsx.f90.bdy"
!!--end--
END FUNCTION


END MODULE
