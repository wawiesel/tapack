!!### MODULE: PRINTING subroutine PRN_Array2
MODULE PRN_Array2
!!#### PURPOSE
!! The subroutine <PRINT_Array2> prints the data contained in rank 2
!! array <A> in a versatile table format.

!!#### USAGE
!!
!!    CALL PRINT_Array2(A)
!
!! where <A> is a rank 2 array, <A(1:NX,1:NY)>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4,&
              KIND_S

!!#### EXTERNAL PARAMETERS
USE PAR_Units                                    !!((02-A-PAR_Units.f90))

!!#### EXTERNAL PROCEDURES
USE SUB_CLEAR                                    !!((04-A-SUB_CLEAR.f90))
USE FUN_STR                                      !!((05-B-FUN_STR.f90))
USE FUN_FieldWidth                               !!((07-B-FUN_FieldWidth.f90))
USE FUN_INT                                      !!((06-B-FUN_INT.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### OPTIONAL ARGUMENTS
!! @ Change the output unit [UNIT]
!! @ Change the output format for a single element [FMT]
!! @ Set the x-header [xHDR(1:NX)]
!! @ Set the y-header [yHDR(0:NY)]
!! @ Set the number of spaces between groups [NSEP]
!! @ Set the maximum number of values per line [NPER]


!!#### EXAMPLE
!! The following output would result from NX=8, NPER=6, NY=3, and NSEP=1.
!
!!  *************************************************************************
!!  * yHDR(0) *  xHDR(1)  *  xHDR(2)  *  xHDR(3)  * . . . *  xHDR(NPER)  *
!!  *************************************************************************
!!  * yHDR(1) *   A(1,1)  |   A(2,1)  |   A(3,1)  | . . . |   A(NPER,1)  |
!!  ***********--------------------------------------------------------------
!!  * yHDR(2) *   A(1,2)  |   A(2,2)  |   A(3,2)  | . . . |   A(NPER,2)  |
!!  ***********--------------------------------------------------------------
!!  * yHDR(3) *   A(1,3)  |   A(2,3)  |   A(3,3)  | . . . |   A(NPER,3)  |
!!  ***********--------------------------------------------------------------
!!  -------------------------------------------------------------------------
!!                           NSEP       s  p  a  c  e  s
!!  -------------------------------------------------------------------------
!!  ***********************************
!!  * yHDR(0) *  xHDR(7)  *  xHDR(8)  *
!!  ***********************************
!!  * yHDR(1) *   A(7,1)  |   A(8,1)  |
!!  ***********------------------------
!!  * yHDR(2) *   A(7,2)  |   A(8,2)  |
!!  ***********------------------------
!!  * yHDR(3) *   A(7,3)  |   A(8,3)  |
!!  ***********------------------------
!
!!  Note that the borders are not actually printed (*,|,- characters.)

!!#### PROCEDURE OVERLOADING
INTERFACE PRINT_Array2
 MODULE PROCEDURE PRINT_Array2_Rsp
 MODULE PROCEDURE PRINT_Array2_Rdp
 MODULE PROCEDURE PRINT_Array2_Csp
 MODULE PROCEDURE PRINT_Array2_Cdp
 MODULE PROCEDURE PRINT_Array2_I1
 MODULE PROCEDURE PRINT_Array2_I2
 MODULE PROCEDURE PRINT_Array2_I4
 MODULE PROCEDURE PRINT_Array2_I8
 MODULE PROCEDURE PRINT_Array2_L1
 MODULE PROCEDURE PRINT_Array2_L2
 MODULE PROCEDURE PRINT_Array2_L4
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: PRINT_Array2

!!#### LOCAL PARAMETERS
!! @ number of characters allowed in the string representation of
!!   the number of entries in the first dimension of the array [LEN_N]
!! @ number of characters allowed in the format for a single
!!   entry of the array [LEN_FMT]
INTEGER,PARAMETER :: LEN_N   = 10
INTEGER,PARAMETER :: LEN_FMT = 13
!*note* both of are overkill to ensure we have enough characters---
!you would need an extent of 10,000,000,000 in the first dimension to exceed
!LEN_N=10 and a formatting statement such as ABC98765.4321 to exceed LEN_FMT=13.

CONTAINS

SUBROUTINE PRINT_Array2_Rsp(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_R = KIND_Rsp
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "Es12.3"
INCLUDE "08-B-PRN_Array2_R.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Array2_Rdp(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_R = KIND_Rdp
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "Es13.4"
INCLUDE "08-B-PRN_Array2_R.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE PRINT_Array2_Csp(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_C = KIND_Csp
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "Es12.3"
INCLUDE "08-B-PRN_Array2_C.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Array2_Cdp(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_C = KIND_Cdp
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "Es13.4"
INCLUDE "08-B-PRN_Array2_C.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE



SUBROUTINE PRINT_Array2_I1(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_I = KIND_I1
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "I3.1"
INCLUDE "08-B-PRN_Array2_I.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Array2_I2(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_I = KIND_I2
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "I4.1"
INCLUDE "08-B-PRN_Array2_I.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Array2_I4(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_I = KIND_I4
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "I7.1"
INCLUDE "08-B-PRN_Array2_I.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Array2_I8(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_I = KIND_I8
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "I8.1"
INCLUDE "08-B-PRN_Array2_I.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE



SUBROUTINE PRINT_Array2_L1(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_L = KIND_L1
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "L2.1"
INCLUDE "08-B-PRN_Array2_L.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Array2_L2(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_L = KIND_L2
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "L2.1"
INCLUDE "08-B-PRN_Array2_L.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Array2_L4(A,UNIT,FMT,NPER,NSEP,xHDR,yHDR)
!!#### PARAMETERS
INTEGER     ,PARAMETER :: KIND_L = KIND_L4
CHARACTER(*),PARAMETER :: DEFAULT_FMT  = "L2.1"
INCLUDE "08-B-PRN_Array2_L.f90.hdr"
INCLUDE "08-B-PRN_Array2.f90.hdr"
!!--begin--
INCLUDE "08-B-PRN_Array2.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
