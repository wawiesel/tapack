MODULE SUB_Sort_quick
!!#### PURPOSE
!! Performs the standard Quick Sort algorithm
!! (or the simple Interchange Sort if the list is small)
!! on a list of integers, reals, or strings.


!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp,& !!((01-A-KND_IntrinsicTypes.f90))
              KIND_I1,KIND_I2,KIND_I4,KIND_I8,&
              KIND_S


!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFAULT ACCESS
PRIVATE

!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: DEFAULT_max_simple_sort_size = 6

!!#### PROCEDURE OVERLOADING
INTERFACE Sort_quick
 MODULE PROCEDURE Sort_quick_Rsp
 MODULE PROCEDURE Sort_quick_Rdp
 MODULE PROCEDURE Sort_quick_I1
 MODULE PROCEDURE Sort_quick_I2
 MODULE PROCEDURE Sort_quick_I4
 MODULE PROCEDURE Sort_quick_I8
 MODULE PROCEDURE Sort_quick_S
END INTERFACE

!!#### PROCEDURE OVERLOADING
INTERFACE Sort_quick_
 MODULE PROCEDURE Sort_quick__Rsp
 MODULE PROCEDURE Sort_quick__Rdp
 MODULE PROCEDURE Sort_quick__I1
 MODULE PROCEDURE Sort_quick__I2
 MODULE PROCEDURE Sort_quick__I4
 MODULE PROCEDURE Sort_quick__I8
 MODULE PROCEDURE Sort_quick__S
END INTERFACE

!!#### PUBLIC ACCESS LIST
PUBLIC :: Sort_quick


CONTAINS


PURE SUBROUTINE Sort_quick_Rsp(list,order,max_simple_sort_size)
INTEGER,PARAMETER ::  KIND_R=KIND_Rsp
INCLUDE "03-A-SUB_Sort_quick_R.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Sort_quick.f90.bdy"
!!--end--
END SUBROUTINE Sort_quick_Rsp


PURE SUBROUTINE Sort_quick_Rdp(list,order,max_simple_sort_size)
INTEGER,PARAMETER ::  KIND_R=KIND_Rdp
INCLUDE "03-A-SUB_Sort_quick_R.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Sort_quick.f90.bdy"
!!--end--
END SUBROUTINE Sort_quick_Rdp


PURE SUBROUTINE Sort_quick_S(list,order,max_simple_sort_size)
INCLUDE "03-A-SUB_Sort_quick_S.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Sort_quick.f90.bdy"
!!--end--
END SUBROUTINE Sort_quick_S


PURE SUBROUTINE Sort_quick_I1(list,order,max_simple_sort_size)
INTEGER,PARAMETER ::  KIND_I=KIND_I1
INCLUDE "03-A-SUB_Sort_quick_I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Sort_quick.f90.bdy"
!!--end--
END SUBROUTINE Sort_quick_I1


PURE SUBROUTINE Sort_quick_I2(list,order,max_simple_sort_size)
INTEGER,PARAMETER ::  KIND_I=KIND_I2
INCLUDE "03-A-SUB_Sort_quick_I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Sort_quick.f90.bdy"
!!--end--
END SUBROUTINE Sort_quick_I2


PURE SUBROUTINE Sort_quick_I4(list,order,max_simple_sort_size)
INTEGER,PARAMETER ::  KIND_I=KIND_I4
INCLUDE "03-A-SUB_Sort_quick_I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Sort_quick.f90.bdy"
!!--end--
END SUBROUTINE Sort_quick_I4


PURE SUBROUTINE Sort_quick_I8(list,order,max_simple_sort_size)
INTEGER,PARAMETER ::  KIND_I=KIND_I8
INCLUDE "03-A-SUB_Sort_quick_I.f90.hdr"
!!--begin--
INCLUDE "03-A-SUB_Sort_quick.f90.bdy"
!!--end--
END SUBROUTINE Sort_quick_I8


PURE RECURSIVE SUBROUTINE Sort_quick__Rsp( list , order , left_end , right_end , max_simple_sort_size_ )
INTEGER,PARAMETER ::  KIND_R=KIND_Rsp
INCLUDE "03-A-SUB_Sort_quick__R.f90.hdr"
!
INCLUDE "03-A-SUB_Sort_quick_.f90.bdy"
!
END SUBROUTINE

PURE RECURSIVE SUBROUTINE Sort_quick__Rdp( list , order , left_end , right_end , max_simple_sort_size_ )
INTEGER,PARAMETER ::  KIND_R=KIND_Rdp
INCLUDE "03-A-SUB_Sort_quick__R.f90.hdr"
!
INCLUDE "03-A-SUB_Sort_quick_.f90.bdy"
!
END SUBROUTINE

PURE RECURSIVE SUBROUTINE Sort_quick__I1( list , order , left_end , right_end , max_simple_sort_size_ )
INTEGER,PARAMETER ::  KIND_I=KIND_I1
INCLUDE "03-A-SUB_Sort_quick__I.f90.hdr"
!
INCLUDE "03-A-SUB_Sort_quick_.f90.bdy"
!
END SUBROUTINE

PURE RECURSIVE SUBROUTINE Sort_quick__I2( list , order , left_end , right_end , max_simple_sort_size_ )
INTEGER,PARAMETER ::  KIND_I=KIND_I2
INCLUDE "03-A-SUB_Sort_quick__I.f90.hdr"
!
INCLUDE "03-A-SUB_Sort_quick_.f90.bdy"
!
END SUBROUTINE

PURE RECURSIVE SUBROUTINE Sort_quick__I4( list , order , left_end , right_end , max_simple_sort_size_ )
INTEGER,PARAMETER ::  KIND_I=KIND_I4
INCLUDE "03-A-SUB_Sort_quick__I.f90.hdr"
!
INCLUDE "03-A-SUB_Sort_quick_.f90.bdy"
!
END SUBROUTINE

PURE RECURSIVE SUBROUTINE Sort_quick__I8( list , order , left_end , right_end , max_simple_sort_size_ )
INTEGER,PARAMETER ::  KIND_I=KIND_I8
INCLUDE "03-A-SUB_Sort_quick__I.f90.hdr"
!
INCLUDE "03-A-SUB_Sort_quick_.f90.bdy"
!
END SUBROUTINE

PURE RECURSIVE SUBROUTINE Sort_quick__S( list , order , left_end , right_end , max_simple_sort_size_ )
INCLUDE "03-A-SUB_Sort_quick__S.f90.hdr"
!
INCLUDE "03-A-SUB_Sort_quick_.f90.bdy"
!
END SUBROUTINE


END MODULE
