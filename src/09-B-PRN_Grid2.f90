!!### MODULE: PRINTING subroutine Grid2
MODULE PRN_Grid2
!!#### PURPOSE
!! This subroutine prints a block of discrete function values
!! F(1:Nx,1:Ny) provided at points on a regular grid, or just with indices
!! if the values of <x(1:Nx)> and <y(1:Ny)> are not provided.

!!#### FORTRAN STANDARDS
USE ISO_varying_string                                !!((03-A-ISO_varying_string.f90))

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,KIND_S !!((01-A-KND_IntrinsicTypes.f90))

!!#### GLOBAL UNITS
USE PAR_IntrinsicLengths  ,ONLY: LEN_Rsp,LEN_Rdp      !!((02-A-PAR_IntrinsicLengths.f90))
USE PAR_Units,ONLY: window_unit                       !!((02-A-PAR_Units.f90))

!!#### GLOBAL USER MODULES
USE USR_fdbk                                          !!((08-C-USR_fdbk.f90))

!!#### BINARY OPERATORS
USE BOP_Concatenate                                   !!((03-A-BOP_Concatenate.f90))

!!#### GLOBAL FUNCTIONS
USE FUN_STR                                           !!((05-B-FUN_STR.f90))
USE FUN_Sequence                                      !!((03-A-FUN_Sequence.f90))
USE FUN_Default                                       !!((04-A-FUN_Default.f90))
USE FUN_FMT                                           !!((06-B-FUN_FMT.f90))
USE FUN_VSTR                                          !!((05-B-FUN_VSTR.f90))
USE SUB_CLEAR                                         !!((04-A-SUB_CLEAR.f90))

!!#### GLOBAL PRINTING
USE PRN_Array2                                        !!((08-B-PRN_Array2.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: DEFAULT_unit  = window_unit

!!#### PROCEDURE OVERLOADING
INTERFACE PRINT_Grid2
 MODULE PROCEDURE PRINT_Grid2_Rsp
 MODULE PROCEDURE PRINT_Grid2_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: PRINT_Grid2

CONTAINS


SUBROUTINE PRINT_Grid2_Rsp( Z , Unit , x , y )
!!#### PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp         !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_R = LEN_Rsp
INCLUDE "09-B-PRN_Grid2_R.f90.hdr"
!!--begin--
INCLUDE "09-B-PRN_Grid2_R.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE PRINT_Grid2_Rdp( Z , Unit , x , y )
!!#### PARAMETERS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp         !!((01-A-KND_IntrinsicTypes.f90))
INTEGER,PARAMETER :: LEN_R = LEN_Rdp
INCLUDE "09-B-PRN_Grid2_R.f90.hdr"
!!--begin--
INCLUDE "09-B-PRN_Grid2_R.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
