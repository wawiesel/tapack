!!## LIBRARY MODULE: ALLOCATE_Shape
MODULE LIB_ALLOCATE_Shape

!!### PURPOSE
!! Allocate a shape.


!!#### USAGE
!!  CALL ALLOCATE_Shape( X )
!
!! where <Shape> is the name of the shape and <X> is a pointer of
!! rank required for that shape or 1D.


!!#### AUTHORS|CONTACT INFO
!! W. A. Wieselquist [waw] | william.wieselquist@gmail.com


!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!#### EXTERNAL PARAMETERS
USE PAR_ComputationalGeometry                  !!((02-A-PAR_ComputationalGeometry.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### PROCEDURE OVERLOADING
INTERFACE ALLOCATE_Pn
 MODULE PROCEDURE ALLOCATE_Pn_Rsp
 MODULE PROCEDURE ALLOCATE_Pn_Rdp
END INTERFACE
INTERFACE ALLOCATE_Ls
 MODULE PROCEDURE ALLOCATE_Ls_Rsp
 MODULE PROCEDURE ALLOCATE_Ls_Rdp
END INTERFACE
INTERFACE ALLOCATE_Lsx
 MODULE PROCEDURE ALLOCATE_Lsx_Rsp
 MODULE PROCEDURE ALLOCATE_Lsx_Rdp
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: ALLOCATE_Pn
PUBLIC :: ALLOCATE_Ls
PUBLIC :: ALLOCATE_Lsx

CONTAINS


PURE SUBROUTINE ALLOCATE_Pn_Rsp( NDim , Coord , Pn )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-LIB_ALLOCATE_Shape__Pn.f90.hdr"
!!--begin--
INCLUDE "03-A-LIB_ALLOCATE_Shape__Pn.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE ALLOCATE_Pn_Rdp( NDim , Coord , Pn )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-LIB_ALLOCATE_Shape__Pn.f90.hdr"
!!--begin--
INCLUDE "03-A-LIB_ALLOCATE_Shape__Pn.f90.bdy"
!!--end--
END SUBROUTINE


PURE SUBROUTINE ALLOCATE_Ls_Rsp( NDim , Coord , Ls )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-LIB_ALLOCATE_Shape__Ls.f90.hdr"
!!--begin--
INCLUDE "03-A-LIB_ALLOCATE_Shape__Ls.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE ALLOCATE_Ls_Rdp( NDim , Coord , Ls )
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-LIB_ALLOCATE_Shape__Ls.f90.hdr"
!!--begin--
INCLUDE "03-A-LIB_ALLOCATE_Shape__Ls.f90.bdy"
!!--end--
END SUBROUTINE


PURE SUBROUTINE ALLOCATE_Lsx_Rsp( NDim , Coord , Lsx , N)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-LIB_ALLOCATE_Shape__Lsx.f90.hdr"
!!--begin--
INCLUDE "03-A-LIB_ALLOCATE_Shape__Lsx.f90.bdy"
!!--end--
END SUBROUTINE

PURE SUBROUTINE ALLOCATE_Lsx_Rdp( NDim , Coord , Lsx , N)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-LIB_ALLOCATE_Shape__Lsx.f90.hdr"
!!--begin--
INCLUDE "03-A-LIB_ALLOCATE_Shape__Lsx.f90.bdy"
!!--end--
END SUBROUTINE


END MODULE
