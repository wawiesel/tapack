MODULE PRN_Function1
!!#### PURPOSE
!! Print a simple function of x on the terminal.

!!#### INFO
!! author : William Wieselquist [waw]
!! contact: william.wieselquist@gmail.com

!!#### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))
USE KND_IntrinsicTypes,ONLY: KIND_I            !!((01-A-KND_IntrinsicTypes.f90))

!!#### DEFAULT IMPLICIT
IMPLICIT NONE

!!#### DEFUALT ACCESS
PRIVATE


!!#### PROCEDURE OVERLOADING
INTERFACE PRINT_Function1
 MODULE PROCEDURE PRINT_Function1_Rsp
 MODULE PROCEDURE PRINT_Function1_Rdp
END INTERFACE

!!#### LOCAL PARAMETERS
INTEGER,PARAMETER :: ISCR=60,JSCR=21

!!#### LOCAL VARIABLES
CHARACTER,SAVE :: blank=" ",zero="=",yy="|",xx="-",ff="+"

!!#### PUBLIC ACCESS LIST
PUBLIC :: PRINT_Function1

CONTAINS

SUBROUTINE PRINT_Function1_Rsp(f,domain,range)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function1_Rsp.f90.hdr"
INCLUDE "05-B-PRN_Function1.f90.hdr"
!!--begin--
INCLUDE "05-B-PRN_Function1.f90.bdy"
!!--end--
END SUBROUTINE

SUBROUTINE PRINT_Function1_Rdp(f,domain,range)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "03-A-INT_Function1_Rdp.f90.hdr"
INCLUDE "05-B-PRN_Function1.f90.hdr"
!!--begin--
INCLUDE "05-B-PRN_Function1.f90.bdy"
!!--end--
END SUBROUTINE

END MODULE
