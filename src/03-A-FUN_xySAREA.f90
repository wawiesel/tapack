MODULE FUN_xySAREA
!!#### PURPOSE
!! Calculate the signed SAREA of various shapes:
!!  @ quadrilateral [Qg]
!!  @ Trangle <Tr>
!!  @ polygon <Po>

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp       !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_1_by_2_Rsp => c_1_by_2 !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_1_by_2_Rdp => c_1_by_2 !!((02-A-PAR_Constants_Rdp.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE xySAREA_Tr
 MODULE PROCEDURE xySAREA_Tr_Rsp
 MODULE PROCEDURE xySAREA_Tr_Rdp
END INTERFACE

INTERFACE xySAREA_Qg
 MODULE PROCEDURE xySAREA_Qg_Rsp
 MODULE PROCEDURE xySAREA_Qg_Rdp
END INTERFACE

INTERFACE xySAREA_Pg
 MODULE PROCEDURE xySAREA_Pg_Rsp
 MODULE PROCEDURE xySAREA_Pg_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: xySAREA_Tr
PUBLIC :: xySAREA_Qg
PUBLIC :: xySAREA_Pg


CONTAINS


PURE FUNCTION xySAREA_Tr_Rsp( Tr ) RESULT(SAREA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rsp
INCLUDE "03-A-FUN_xySAREA_Tr.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySAREA_Tr.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xySAREA_Tr_Rdp( Tr ) RESULT(SAREA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rdp
INCLUDE "03-A-FUN_xySAREA_Tr.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySAREA_Tr.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xySAREA_Qg_Rsp( Qg ) RESULT(SAREA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rsp
INCLUDE "03-A-FUN_xySAREA_Qg.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySAREA_Qg.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xySAREA_Qg_Rdp( Qg ) RESULT(SAREA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rdp
INCLUDE "03-A-FUN_xySAREA_Qg.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySAREA_Qg.f90.bdy"
!!--end--
END FUNCTION


PURE FUNCTION xySAREA_Pg_Rsp( N , Pg ) RESULT(SAREA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rsp
INCLUDE "03-A-FUN_xySAREA_Pg.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySAREA_Pg.f90.bdy"
!!--end--
END FUNCTION

PURE FUNCTION xySAREA_Pg_Rdp( N , Pg ) RESULT(SAREA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp        !!((01-A-KND_IntrinsicTypes.f90))
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1_by_2 = c_1_by_2_Rdp
INCLUDE "03-A-FUN_xySAREA_Pg.f90.hdr"
!!--begin--
INCLUDE "03-A-FUN_xySAREA_Pg.f90.bdy"
!!--end--
END FUNCTION


END MODULE
