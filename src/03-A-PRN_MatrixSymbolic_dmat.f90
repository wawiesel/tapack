MODULE PRN_MatrixSymbolic_dmat
!!#### PURPOSE
!! Prints a dense matrix (<dmat>) in a special
!! symbolic form and optionally some statistics about it.


!!#### USAGE
!
!   CALL PRINT_MatrixSymbolic( A )
!
!! where A is a dmat real or complex matrix (rank-2 array).


!!#### DETAILS
!! For real matrices:
!!   @ zero elements are represented with character "0",
!!   @ unity elements are represented with elements "1",
!!   @ and all other values are displayed as "*".
!! For complex matrices the real part and the complex part are both displayed
!! as "a,b" where a="0","1",or "*" and b="0","1", or "*".
!! The diagonals are highlighted by surrounding with characters "(" and ")".

!!#### OPTIONAL ARGUMENTS
!! @ Change the delimeter character for complex matrices <Delimeter>
!! @ Change the zero character <Zero>
!! @ Change the unity character <Unity>
!! @ Change the other character <Other>
!! @ Change the unit <Unit>
!! @ Whether to output statistics <Stats>
!! @ Change the left diagonal highlight character <LHighlight>
!! @ Change the right diagonal highlight character <RHighlight>

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_Csp,KIND_Cdp

!!#### EXTERNAL PARAMETERS
USE PAR_Units,ONLY: window_unit                  !!((02-A-PAR_Units.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PARAMETERS
INTEGER  ,PARAMETER :: DEFAULT_unit  = window_unit
LOGICAL  ,PARAMETER :: DEFAULT_stats = .TRUE.
CHARACTER,PARAMETER :: DEFAULT_zero = "0"
CHARACTER,PARAMETER :: DEFAULT_unity = "1"
CHARACTER,PARAMETER :: DEFAULT_other = "*"
CHARACTER,PARAMETER :: DEFAULT_delimeter = ","
CHARACTER,PARAMETER :: DEFAULT_lhighlight = "("
CHARACTER,PARAMETER :: DEFAULT_rhighlight = ")"

!!#### INTERFACE to PRINT_MatrixSymbolic
INTERFACE PRINT_MatrixSymbolic
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Rsp
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Rdp
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Csp
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Cdp
END INTERFACE
INTERFACE PRINT_MatrixSymbolic_dmat
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Rsp
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Rdp
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Csp
 MODULE PROCEDURE PRINT_MatrixSymbolic_dmat_Cdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: PRINT_MatrixSymbolic_dmat

CONTAINS

SUBROUTINE PRINT_MatrixSymbolic_dmat_Rsp(A,unit,stats,zero,unity,other,&
  lhighlight,rhighlight)
INTEGER,PARAMETER :: KIND_A = KIND_Rsp
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_R.f90.hdr"
!!--begin--
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_R.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_MatrixSymbolic_dmat_Rdp(A,unit,stats,zero,unity,other,&
  lhighlight,rhighlight)
INTEGER,PARAMETER :: KIND_A = KIND_Rdp
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_R.f90.hdr"
!!--begin--
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_R.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE PRINT_MatrixSymbolic_dmat_Csp(A,unit,stats,zero,unity,other,&
  lhighlight,rhighlight,delimeter)
INTEGER,PARAMETER :: KIND_A = KIND_Csp
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_C.f90.hdr"
!!--begin--
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_C.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_MatrixSymbolic_dmat_Cdp(A,unit,stats,zero,unity,other,&
  lhighlight,rhighlight,delimeter)
INTEGER,PARAMETER :: KIND_A = KIND_Cdp
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_C.f90.hdr"
!!--begin--
INCLUDE "03-A-PRN_MatrixSymbolic_dmat_C.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
