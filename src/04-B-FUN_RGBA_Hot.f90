!!## FUNCTION: RGBA_Hot
MODULE FUN_RGBA_Hot
!!### PURPOSE
!! Get a Hot (yellow->orange->red->pink) color scheme in
!! RGBA format with components <R,G,B,A> all in $[0,1]$.


!!### USAGE
!
!      RGBA = RGBA_Hot( X )
!
!! where <RGBA(1:4)> is an array containing each of the components
!! in that order, <X> is the input intensity to generate the color.


!!### EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rsp,KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))

!!### DEFAULT IMPLICIT
IMPLICIT NONE

!!### DEFAULT ACCESS
PRIVATE

!!### PROCEDURE OVERLOADING
INTERFACE RGBA_Hot
 MODULE PROCEDURE RGBA_Hot_Rsp
 MODULE PROCEDURE RGBA_Hot_Rdp
END INTERFACE

!!### PUBLIC ACCESS LIST
PUBLIC :: RGBA_Hot


!!## MODULE PROCEDURES
CONTAINS

!!### PURE FUNCTION: RGBA_Hot_Rsp
PURE FUNCTION RGBA_Hot_Rsp( X ) RESULT(RGBA)
!!#### LOCAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rsp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_RGBA_Hot.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_RGBA_Hot.f90.bdy"
!!--end--
END FUNCTION


!!### PURE FUNCTION: RGBA_Hot_Rdp
PURE FUNCTION RGBA_Hot_Rdp( X ) RESULT(RGBA)
USE KND_IntrinsicTypes,ONLY: KIND_R=>KIND_Rdp  !!((01-A-KND_IntrinsicTypes.f90))
INCLUDE "04-B-FUN_RGBA_Hot.f90.hdr"
!!--begin--
INCLUDE "04-B-FUN_RGBA_Hot.f90.bdy"
!!--end--
END FUNCTION


END MODULE
