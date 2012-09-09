MODULE SUB_Shake
!!#### PURPOSE
!! Randomly "shakes" the elements of a rank 1 array,
!! changing the order randomly but not effecting the values
!! themselves.

!!#### METHOD
!! Repeatedly calls <Random(1,1,M)>, where <M=SIZE(ARRAY)> to
!! get two indices to swap.  The optional argument <NUM> controls
!! how many swaps to make.  The default is <M> times for
!! <2*M> total calls to the random number generator (Expensive!).

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4,&
              KIND_S

!!#### FORTRAN STANDARDS
USE ISO_varying_string                           !!((03-A-ISO_varying_string.f90))

!!#### EXTERNAL PROCEDURES
USE FUN_Random                                   !!((03-A-FUN_Random.f90))
USE SUB_Swap                                     !!((04-A-SUB_Swap.f90))
USE FUN_Default                                  !!((04-A-FUN_Default.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### SUBROUTINE OVERLOADING
INTERFACE Shake
 MODULE PROCEDURE Shake_L1
 MODULE PROCEDURE Shake_L2
 MODULE PROCEDURE Shake_L4
 MODULE PROCEDURE Shake_I1
 MODULE PROCEDURE Shake_I2
 MODULE PROCEDURE Shake_I4
 MODULE PROCEDURE Shake_I8
 MODULE PROCEDURE Shake_Rsp
 MODULE PROCEDURE Shake_Rdp
 MODULE PROCEDURE Shake_Csp
 MODULE PROCEDURE Shake_Cdp
 MODULE PROCEDURE Shake_VS
 MODULE PROCEDURE Shake_S
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Shake


CONTAINS


SUBROUTINE Shake_L1( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_L.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_L2( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_L.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_L4( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_L.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE



SUBROUTINE Shake_I1( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_I.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_I2( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_I.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_I4( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_I.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_I8( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_I.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_Rsp( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_R.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_Rdp( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_R.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_Csp( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_C.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_Cdp( ARRAY , NUM )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "06-B-SUB_Shake_C.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE



SUBROUTINE Shake_S( ARRAY , NUM )
INCLUDE "06-B-SUB_Shake_S.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Shake_VS( ARRAY , NUM )
INCLUDE "06-B-SUB_Shake_VS.f90.hdr"
INCLUDE "06-B-SUB_Shake.f90.hdr"
!!--begin--
INCLUDE "06-B-SUB_Shake.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
