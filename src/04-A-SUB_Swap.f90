MODULE SUB_Swap
!!#### PURPOSE
!! Swaps the two values of the two arguments.

!!#### USAGE
!! The code snippet
!
!    x = a
!    y = b
!    CALL Swap( x , y )
!
!! yields <x=b> and <y=a>.

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_Csp,KIND_Cdp,&
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_L1,KIND_L2,KIND_L4,&
              KIND_S

!!#### FORTRAN STANDARDS
USE ISO_varying_string                           !!((03-A-ISO_varying_string.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### SUBROUTINE OVERLOADING
INTERFACE Swap
 MODULE PROCEDURE Swap_L1
 MODULE PROCEDURE Swap_L2
 MODULE PROCEDURE Swap_L4
 MODULE PROCEDURE Swap_I1
 MODULE PROCEDURE Swap_I2
 MODULE PROCEDURE Swap_I4
 MODULE PROCEDURE Swap_I8
 MODULE PROCEDURE Swap_Rsp
 MODULE PROCEDURE Swap_Rdp
 MODULE PROCEDURE Swap_Csp
 MODULE PROCEDURE Swap_Cdp
 MODULE PROCEDURE Swap_VS
 MODULE PROCEDURE Swap_S
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Swap


CONTAINS


SUBROUTINE Swap_L1( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_L.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_L2( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_L.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_L4( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_L=>KIND_L4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_L.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE



SUBROUTINE Swap_I1( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I1     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_I.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_I2( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I2     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_I.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_I4( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I4     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_I.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_I8( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_I=>KIND_I8     !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_I.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_Rsp( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_R.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_Rdp( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_R.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_Csp( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Csp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_C.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_Cdp( x , y )
USE KND_IntrinsicTypes,ONLY: KIND_C=>KIND_Cdp    !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-A-SUB_Swap_C.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE



SUBROUTINE Swap_S( x , y )
INCLUDE "04-A-SUB_Swap_S.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE


SUBROUTINE Swap_VS( x , y )
INCLUDE "04-A-SUB_Swap_VS.f90.hdr"
!!--begin--
INCLUDE "04-A-SUB_Swap.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
