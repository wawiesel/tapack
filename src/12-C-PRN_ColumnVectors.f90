!!#PRINTING MODULE: ColumnVectors
MODULE PRN_ColumnVectors
!!##PURPOSE
!! Prints a list of vectors column-wise.

!!##USAGE
!! Print a rank 2 array assumed to be a list of vectors,
!! by index as row and vector number as column.
!
!  CALL PRINT_ColumnVectors(X)
!

!!##GLOBAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_S,&         !!((01-A-KND_IntrinsicTypes.f90))
                             KIND_Rsp,KIND_Rdp,&
                             KIND_Csp,KIND_Cdp,&
                             KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
                                                         KIND_L1,KIND_L2,KIND_L4

!!##GLOBAL PROCEDURES
USE FUN_Default                               !!((04-A-FUN_Default.f90))
USE FUN_STR                                   !!((05-B-FUN_STR.f90))
USE SUB_CLEAR                                 !!((04-A-SUB_CLEAR.f90))
USE PRN_Table                                 !!((11-B-PRN_Table.f90))

!!##DEFAULT IMPLICIT
IMPLICIT NONE


!!##DEFAULT ACCESS
PRIVATE



!!##PROCEDURE OVERLOADING
INTERFACE PRINT_ColumnVectors
 MODULE PROCEDURE PRINT_ColumnVectors_Rsp
 MODULE PROCEDURE PRINT_ColumnVectors_Rdp
 MODULE PROCEDURE PRINT_ColumnVectors_Csp
 MODULE PROCEDURE PRINT_ColumnVectors_Cdp
 MODULE PROCEDURE PRINT_ColumnVectors_I1
 MODULE PROCEDURE PRINT_ColumnVectors_I2
 MODULE PROCEDURE PRINT_ColumnVectors_I4
 MODULE PROCEDURE PRINT_ColumnVectors_I8
 MODULE PROCEDURE PRINT_ColumnVectors_L1
 MODULE PROCEDURE PRINT_ColumnVectors_L2
 MODULE PROCEDURE PRINT_ColumnVectors_L4
END INTERFACE



!!##PUBLIC ACCESS LIST
PUBLIC :: PRINT_ColumnVectors_Rsp
PUBLIC :: PRINT_ColumnVectors_Rdp
PUBLIC :: PRINT_ColumnVectors_Csp
PUBLIC :: PRINT_ColumnVectors_Cdp
PUBLIC :: PRINT_ColumnVectors_I1
PUBLIC :: PRINT_ColumnVectors_I2
PUBLIC :: PRINT_ColumnVectors_I4
PUBLIC :: PRINT_ColumnVectors_I8
PUBLIC :: PRINT_ColumnVectors_L1
PUBLIC :: PRINT_ColumnVectors_L2
PUBLIC :: PRINT_ColumnVectors_L4
PUBLIC :: PRINT_ColumnVectors



!!##LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_i = "(I)                             "



!!##PROCEDURE LISTING
CONTAINS



!!###SUBROUTINE: PRINT_ColumnVectors_Rsp
SUBROUTINE PRINT_ColumnVectors_Rsp( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for single precision Real <Rsp>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(E)                             "

INCLUDE "12-C-PRN_ColumnVectors_R.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE


!!###SUBROUTINE: PRINT_ColumnVectors_Rdp
SUBROUTINE PRINT_ColumnVectors_Rdp( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for double precision Real <Rdp>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(E)                             "

INCLUDE "12-C-PRN_ColumnVectors_R.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE



!!###SUBROUTINE: PRINT_ColumnVectors_Csp
SUBROUTINE PRINT_ColumnVectors_Csp( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for single precision Complex <Csp>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(E)                             "

INCLUDE "12-C-PRN_ColumnVectors_C.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE



!!###SUBROUTINE: PRINT_ColumnVectors_Cdp
SUBROUTINE PRINT_ColumnVectors_Cdp( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for double precision Complex <Cdp>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(E)                             "

INCLUDE "12-C-PRN_ColumnVectors_C.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE




!!###SUBROUTINE: PRINT_ColumnVectors_I1
SUBROUTINE PRINT_ColumnVectors_I1( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for 1-byte signed integers <I1>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1  !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(I)                            "

INCLUDE "12-C-PRN_ColumnVectors_I.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE


!!###SUBROUTINE: PRINT_ColumnVectors_I2
SUBROUTINE PRINT_ColumnVectors_I2( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for 2-byte signed integers <I2>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2  !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(I)                            "

INCLUDE "12-C-PRN_ColumnVectors_I.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE



!!###SUBROUTINE: PRINT_ColumnVectors_I4
SUBROUTINE PRINT_ColumnVectors_I4( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for 4-byte signed integers <I4>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4  !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(I)                            "

INCLUDE "12-C-PRN_ColumnVectors_I.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE


!!###SUBROUTINE: PRINT_ColumnVectors_I8
SUBROUTINE PRINT_ColumnVectors_I8( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for 8-byte signed integers <I8>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8  !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(I)                            "

INCLUDE "12-C-PRN_ColumnVectors_I.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE



!!###SUBROUTINE: PRINT_ColumnVectors_L1
SUBROUTINE PRINT_ColumnVectors_L1( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for 1-byte signed logicals <L1>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1  !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(G)                            "

INCLUDE "12-C-PRN_ColumnVectors_L.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE


!!###SUBROUTINE: PRINT_ColumnVectors_L2
SUBROUTINE PRINT_ColumnVectors_L2( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for 2-byte logicals <L2>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2  !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(G)                            "

INCLUDE "12-C-PRN_ColumnVectors_L.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE



!!###SUBROUTINE: PRINT_ColumnVectors_L4
SUBROUTINE PRINT_ColumnVectors_L4( x , Unit , NAME_i , NAME_x , NAME_index , FMT_i , FMT_x )
!!####PURPOSE
!! Print column vectors for 4-byte logicals <L4>.

!!####LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4  !!((01-A-KND_IntrinsicTypes.f90))

!!####LOCAL PARAMETERS
CHARACTER(32),PARAMETER :: DEFAULT_FMT_x = "(G)                            "

INCLUDE "12-C-PRN_ColumnVectors_L.f90.hdr"
INCLUDE "12-C-PRN_ColumnVectors.f90.hdr"

!!--begin--

INCLUDE "12-C-PRN_ColumnVectors.f90.bdy"

!!--end--
END SUBROUTINE



END MODULE
