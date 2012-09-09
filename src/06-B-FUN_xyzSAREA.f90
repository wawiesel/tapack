MODULE FUN_xyzSAREA
!!#### PURPOSE
!! Calculate the signed area of various shapes in R^3:
!!  1. quadrilateral [Qd]
!!  2. triangle [Tr]
!!  3. polygon [Pg]

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp             !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_1_by_2_Rsp => c_1_by_2       !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_1_by_2_Rdp => c_1_by_2       !!((02-A-PAR_Constants_Rdp.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_xyzCOLLAPSE,ONLY: xyzCOLLAPSE_Pg                   !!((05-B-FUN_xyzCOLLAPSE.f90))
USE FUN_xySAREA    ,ONLY: xySAREA_Tr,xySAREA_Qg,xySAREA_Pg !!((03-A-FUN_xySAREA.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCES
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xyzSAREA_Tr
 MODULE PROCEDURE xyzSAREA_Tr_Rsp
 MODULE PROCEDURE xyzSAREA_Tr_Rdp
END INTERFACE

INTERFACE xyzSAREA_Qg
 MODULE PROCEDURE xyzSAREA_Qg_Rsp
 MODULE PROCEDURE xyzSAREA_Qg_Rdp
END INTERFACE

INTERFACE xyzSAREA_Pg
 MODULE PROCEDURE xyzSAREA_Pg_Rsp
 MODULE PROCEDURE xyzSAREA_Pg_Rdp
END INTERFACE


!!#### PUBLIC ACCESS LIST
PUBLIC :: xyzSAREA_Tr
PUBLIC :: xyzSAREA_Qg
PUBLIC :: xyzSAREA_Pg


CONTAINS


FUNCTION xyzSAREA_Tr_Rsp( Tr , M32_transform ) RESULT(SAREA)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R      = KIND_Rsp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rsp
INCLUDE "06-B-FUN_xyzSAREA_Tr.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSAREA_Tr.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzSAREA_Tr_Rdp( Tr , M32_transform ) RESULT(SAREA)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R      = KIND_Rdp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rdp
INCLUDE "06-B-FUN_xyzSAREA_Tr.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSAREA_Tr.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzSAREA_Qg_Rsp( Qg , M32_transform ) RESULT(SAREA)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R      = KIND_Rsp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rsp
INCLUDE "06-B-FUN_xyzSAREA_Qg.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSAREA_Qg.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzSAREA_Qg_Rdp( Qg , M32_transform ) RESULT(SAREA)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R      = KIND_Rdp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rdp
INCLUDE "06-B-FUN_xyzSAREA_Qg.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSAREA_Qg.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzSAREA_Pg_Rsp( N , Pg , M32_transform ) RESULT(SAREA)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R      = KIND_Rsp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rsp
INCLUDE "06-B-FUN_xyzSAREA_Pg.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSAREA_Pg.f90.bdy"
!!--end--
END FUNCTION


FUNCTION xyzSAREA_Pg_Rdp( N , Pg , M32_transform ) RESULT(SAREA)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R      = KIND_Rdp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rdp
INCLUDE "06-B-FUN_xyzSAREA_Pg.f90.hdr"
!!--begin--
INCLUDE "06-B-FUN_xyzSAREA_Pg.f90.bdy"
!!--end--
END FUNCTION


END MODULE
